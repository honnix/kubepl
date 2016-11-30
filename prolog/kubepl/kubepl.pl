:- module(kubepl, [
                   initialize/4,        % +URI, +AuthOptions, +HTTPOptions, -Client
                   initialize/5,        % +URI, +Version, +AuthOptions, +HTTPOptions, -Client
                   get_resources/2,     % +Client, -Response
                   get/5,               % +Client, +Entity, +EntityConfig, +Params, -Responsew
                   get/6,               % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   delete/5,            % +Client, +Entity, +EntityConfig, +Params, -Response
                   delete/6,            % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   create/5,            % +Client, +Entity, +EntityConfig, +Params, -Response
                   create/6,            % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   update/5,            % +Client, +Entity, +EntityConfig, +Params, -Response
                   update/6,            % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   partially_update/7,  % +Client, +Entity, +EntityConfig, +Params, +Body, +PatchType, -Response
                   proxy_get/5,         % +Client, +Entity, +EntityConfig, +Params, -Response
                   proxy_get/6,         % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   proxy_delete/5,      % +Client, +Entity, +EntityConfig, +Params, -Responsew
                   proxy_delete/6,      % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   proxy_post/5,        % +Client, +Entity, +EntityConfig, +Params, -Responsew
                   proxy_post/6,        % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   proxy_put/5,         % +Client, +Entity, +EntityConfig, +Params, -Responsew
                   proxy_put/6,         % +Client, +Entity, +EntityConfig, +Params, +Body, -Response
                   proxy_head/5,        % +Client, +Entity, +EntityConfig, +Params, -Responsew
                   proxy_options/5      % +Client, +Entity, +EntityConfig, +Params, -Responsew
                  ]).

/** <module> Kubernetes Client
Check http://kubernetes.io API Reference document for details.

@author Hongxin Liang
@license Apache License Version 2.0
*/

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(ssl)).

%% initialize(+URI, +AuthOptions, +HTTPOptions, -Client) is semidet.
%% initialize(+URI, +Version, +AuthOptions, +HTTPOptions, -Client) is semidet.
%
% Initialize the client.
% 
%  * =URI= points to the API server for example =https://example.com/api=
%  * =Version= indicates Kubernetes API version, for example =v1=; for endpoints
%    not involving a specific API version, use initialize/4 instead
%  * =AuthOptions= could be:
%    * =auth(ssl, SSLOptions)=, refer to ssl:ssl_context/3 for available options
%    * =auth(basic_auth, [user(User),password(Password)])=
%    * =auth(bearer_token [token(Token)])=
%  * Additional HTTP options (except =method, status_code, json_object=) can be
%    passed to underlying http client

initialize(URI, AuthOptions, HTTPOptions, Client) :-
    merge_options(AuthOptions, HTTPOptions, Options),
    Client = kubeplc(_{
                       uri:URI,
                       options:Options
                      }).

initialize(URI, Version, AuthOptions, HTTPOptions, Client) :-
    merge_options(AuthOptions, HTTPOptions, Options),
    Client = kubeplc(_{
                       uri:URI,
                       version:Version,
                       options:Options
                      }).

%% get_resources(+Client, -Response)
%
%  Get available resources.

get_resources(Client, Response) :-
    get(Client, '', _{}, [], _, Response).

%% get(+Client, +Entity, +EntityConfig, +Params, -Response)
%% get(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Get entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

get(Client, Entity, EntityConfig, Params, Response) :-
    get(Client, Entity, EntityConfig, Params, _, Response).

get(kubeplc(Config), Entity, EntityConfig, Params, Body, Response) :-
    build_url(Config, Entity, EntityConfig, Params, URL),
    (   var(Body)
    ->  http_get(URL, Response, [status_code(_),json_object(dict)|Config.options])
    ;   http_post(URL, json(Body), Response,
                  [method(get),status_code(_),json_object(dict)|Config.options])
    ).

%% delete(+Client, +Entity, +EntityConfig, +Params, -Response)
%% delete(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Delete entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

delete(Client, Entity, EntityConfig, Params, Response) :-
    delete(Client, Entity, EntityConfig, Params, _, Response).

delete(kubeplc(Config), Entity, EntityConfig, Params, Body, Response) :-
    build_url(Config, Entity, EntityConfig, Params, URL),
    (   var(Body)
    ->  http_delete(URL, Response, [status_code(_),json_object(dict)|Config.options])
    ;   http_post(URL, json(Body), Response,
                  [method(delete),status_code(_),json_object(dict)|Config.options])
    ).

%% create(+Client, +Entity, +EntityConfig, +Params, -Response)
%% create(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Create entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

create(Client, Entity, EntityConfig, Params, Response) :-
    create(Client, Entity, EntityConfig, Params, _, Response).

create(kubeplc(Config), Entity, EntityConfig, Params, Body, Response) :-
    build_url(Config, Entity, EntityConfig, Params, URL),
    (   var(Body)
    ->  http_post(URL, code(''), Response, [status_code(_),json_object(dict)|Config.options])
    ;   http_post(URL, json(Body), Response,
                 [status_code(_),json_object(dict)|Config.options])
    ).

%% update(+Client, +Entity, +EntityConfig, +Params, -Response)
%% update(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Update entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

update(Client, Entity, EntityConfig, Params, Response) :-
    update(Client, Entity, EntityConfig, Params, _, Response).

update(kubeplc(Config), Entity, EntityConfig, Params, Body, Response) :-
    build_url(Config, Entity, EntityConfig, Params, URL),
    (   var(Body)
    ->  http_put(URL, code(''), Response, [status_code(_),json_object(dict)|Config.options])
    ;   http_put(URL, json(Body), Response,
                 [status_code(_),json_object(dict)|Config.options])
    ).

%% partially_update(+Client, +Entity, +EntityConfig, +Params, +Body, +PatchType, -Response)
%
% Partially update entities, for example =event, limit_range=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% Valid =PatchType= is one of =json=, =merge=, or =strategic-merge=.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

partially_update(kubeplc(Config), Entity, EntityConfig, Params, Body, PatchType, Response) :-
    build_url(Config, Entity, EntityConfig, Params, URL),
    atom_json_term(Codes, Body, [as(codes)]),
    atomic_list_concat(['application/',PatchType,'-patch','+json'], ContentType),
    http_patch(URL, bytes(ContentType, Codes), Response,
               [status_code(_),json_object(dict)|Config.options]).

%% proxy_get(+Client, +Entity, +EntityConfig, +Params, -Response)
%% proxy_get(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Proxy GET request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name}; otherwise pass an empty dictionary.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_get(Client, Entity, EntityConfig, Params, Response) :-
    proxy_get(Client, Entity, EntityConfig, Params, _, Response).

proxy_get(Client, Entity, EntityConfig, Params, Body, Response) :-
    get(Client, Entity, EntityConfig.put(suffix, proxy), Params, Body, Response).

%% proxy_delete(+Client, +Entity, +EntityConfig, +Params, -Response)
%% proxy_delete(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Proxy DELETE request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name}; otherwise pass an empty dictionary.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_delete(Client, Entity, EntityConfig, Params, Response) :-
    proxy_delete(Client, Entity, EntityConfig, Params, _, Response).

proxy_delete(Client, Entity, EntityConfig, Params, Body, Response) :-
    delete(Client, Entity, EntityConfig.put(suffix, proxy), Params, Body, Response).

%% proxy_post(+Client, +Entity, +EntityConfig, +Params, -Response)
%% proxy_post(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Proxy POST request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name}; otherwise pass an empty dictionary.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_post(Client, Entity, EntityConfig, Params, Response) :-
    proxy_post(Client, Entity, EntityConfig, Params, _, Response).

proxy_post(Client, Entity, EntityConfig, Params, Body, Response) :-
    create(Client, Entity, EntityConfig.put(suffix, proxy), Params, Body, Response).

%% proxy_update(+Client, +Entity, +EntityConfig, +Params, -Response)
%% proxy_update(+Client, +Entity, +EntityConfig, +Params, +Body, -Response)
%
% Proxy PUT request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name}; otherwise pass an empty dictionary.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_put(Client, Entity, EntityConfig, Params, Response) :-
    proxy_put(Client, Entity, EntityConfig, Params, _, Response).

proxy_put(Client, Entity, EntityConfig, Params, Body, Response) :-
    update(Client, Entity, EntityConfig.put(suffix, proxy), Params, Body, Response).

%% proxy_head(+Client, +Entity, +EntityConfig, +Params, -Response)
%
% Proxy HEAD request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_head(kubeplc(Config), Entity, EntityConfig, Params, Response) :-
    build_url(Config, Entity, EntityConfig.put(suffix, proxy), Params, URL),
    http_get(URL, Response, [method(head),status_code(_),json_object(dict)|Config.options]).

%% proxy_options(+Client, +Entity, +EntityConfig, +Params, -Response)
%
% Proxy OPTIONS request towards entities, for example =namespace, pods, services=, etc.
%
% For entity who needs additional configuration, use =EntityConfig=, for example
% _{namespace:Namespace, name:Name, suffix:Suffix}; otherwise pass an empty dictionary.
% =Suffix= can be arbitrary atom including a slash-separated context for example
% =some/context=.
%
% For all valid query parameters, please refer to Kubernetes API Reference document.
%
% =Body= must be a valid dictionary according to the entity spec.
%
% HTTP status code will be ignored becaise Kubernetes encodes whatever message in
% =Response=.

proxy_options(kubeplc(Config), Entity, EntityConfig, Params, Response) :-
    build_url(Config, Entity, EntityConfig.put(suffix, proxy), Params, URL),
    http_get(URL, Response, [method(options),status_code(_),json_object(dict)|Config.options]).

merge_options(auth(basic_auth, Options), HTTPOptions, MergedOptions) :- !,
    memberck(user(User), Options),
    memberck(password(Password), Options),
    MergedOptions = [authorization(basic(User, Password))|HTTPOptions].

merge_options(auth(bearer_token, Options), HTTPOptions, MergedOptions) :- !,
    [token(BearerToken)] = Options,
    atom_concat('Bearer ', BearerToken, Value),
    MergedOptions = [request_header('Authorization'=Value)|HTTPOptions].

merge_options(auth(ssl, Options), HTTPOptions, MergedOptions) :- !,
    append(HTTPOptions, Options, MergedOptions).

merge_options(auth(none), HTTPOptions, HTTPOptions).

build_url(Config, Entity, EntityConfig, Params, URL) :-
    add_version(Config, URL0),
    add_namespace(URL0, EntityConfig, URL1),
    atomic_list_concat([URL1,Entity], '/', URL2),
    add_entity_property(URL2, EntityConfig, name, URL3),
    add_entity_property(URL3, EntityConfig, suffix, URL4),
    (   Params \= []
    ->  uri_query_components(Query, Params),
        atomic_list_concat([URL4,'?',Query], URL)
    ;   URL = URL4
    ),
    debug(kubepl, 'URL ~w', [URL]).

add_version(Config, URL) :-
    (   Version = Config.get(version)
    ->  atomic_list_concat([Config.uri,Version], '/', URL)
    ;   URL = Config.uri
    ).

add_namespace(URL0, EntityConfig, URL) :-
    (   Namespace = EntityConfig.get(namespace)
    ->  atomic_list_concat([URL0,namespaces,Namespace], '/', URL)
    ;   URL = URL0
    ).

add_entity_property(URL0, EntityConfig, PropertyName, URL) :-
    (   Property = EntityConfig.get(PropertyName)
    ->  atomic_list_concat([URL0,Property], '/', URL)
    ;   URL = URL0
    ).
