:- module(ex1, []).

:- use_module(library(kubepl/sugar)).
:- use_module(library(kubepl/kubepl)).

:- debug(ex1).

ex1_1(IP) :-
    %% don't ask me why they are here
    expand_file_name('~/Developer/ms_store/ssl/ca.pem', [CA]),
    expand_file_name('~/Developer/ms_store/ssl/admin.pem', [Cert]),
    expand_file_name('~/Developer/ms_store/ssl/admin-key.pem', [Key]),
    atomic_list_concat(['https://',IP,'/api'], URI),
    initialize(URI,
               'v1',
               auth(ssl, [
                          cacert_file(CA),
                          certificate_file(Cert),
                          key_file(Key),
                          cert_verify_hook(cert_accept_any)
                         ]),
               [],
               Client),
    Client=>create(namespaces, _{}, [pretty=true], _{metadata:_{name:test1}}, Response),
    debug(ex1, 'Response ~w', [Response]).

ex1_2(IP) :-
    %% don't ask me why they are here
    expand_file_name('~/Developer/ms_store/ssl/ca.pem', [CA]),
    expand_file_name('~/Developer/ms_store/ssl/admin.pem', [Cert]),
    expand_file_name('~/Developer/ms_store/ssl/admin-key.pem', [Key]),
    atomic_list_concat(['https://',IP,'/api'], URI),
    initialize(URI,
               auth(ssl, [
                          cacert_file(CA),
                          certificate_file(Cert),
                          key_file(Key),
                          cert_verify_hook(cert_accept_any)
                         ]),
               [],
               Client),
    Client=>get('', _{}, [pretty=true], Response),
    debug(ex1, 'Response ~w', [Response]).

ex1_3(IP) :-
    %% don't ask me why they are here
    expand_file_name('~/Developer/ms_store/ssl/ca.pem', [CA]),
    expand_file_name('~/Developer/ms_store/ssl/admin.pem', [Cert]),
    expand_file_name('~/Developer/ms_store/ssl/admin-key.pem', [Key]),
    atomic_list_concat(['https://',IP,'/apis'], URI),
    initialize(URI,
               auth(ssl, [
                          cacert_file(CA),
                          certificate_file(Cert),
                          key_file(Key),
                          cert_verify_hook(cert_accept_any)
                         ]),
               [],
               Client),
    Client=>get('', _{}, [pretty=true], Response1),
    debug(ex1, 'Response ~w', [Response1]),
    Client=>get('extensions', _{}, [pretty=true], Response2),
    debug(ex1, 'Response ~w', [Response2]).

ex1_4(IP) :-
    %% don't ask me why they are here
    expand_file_name('~/Developer/ms_store/ssl/ca.pem', [CA]),
    expand_file_name('~/Developer/ms_store/ssl/admin.pem', [Cert]),
    expand_file_name('~/Developer/ms_store/ssl/admin-key.pem', [Key]),
    atomic_list_concat(['https://',IP,'/version'], URI),
    initialize(URI,
               auth(ssl, [
                          cacert_file(CA),
                          certificate_file(Cert),
                          key_file(Key),
                          cert_verify_hook(cert_accept_any)
                         ]),
               [],
               Client),
    Client=>get('', _{}, [pretty=true], Response),
    debug(ex1, 'Response ~w', [Response]).

ex1_5(IP) :-
    %% don't ask me why they are here
    expand_file_name('~/Developer/ms_store/ssl/ca.pem', [CA]),
    expand_file_name('~/Developer/ms_store/ssl/admin.pem', [Cert]),
    expand_file_name('~/Developer/ms_store/ssl/admin-key.pem', [Key]),
    atomic_list_concat(['https://',IP,'/api'], URI),
    initialize(URI,
               'v1',
               auth(ssl, [
                          cacert_file(CA),
                          certificate_file(Cert),
                          key_file(Key),
                          cert_verify_hook(cert_accept_any)
                         ]),
               [],
               Client),
    Client=>proxy_get(pods, _{namespace:'ms-store', name:wso2am}, [path='test'], Response),
    debug(ex1, 'Response ~w', [Response]).
