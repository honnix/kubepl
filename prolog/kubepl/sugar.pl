:- module(sugar, [
                  op(900, xfx, '=>'),
                  '=>'/2        % +Ps, +Term, -Result
                 ]).

/** <module> syntactic sugar

@author Hongxin Liang
@license Apache License Version 2.0
*/

%% '=>'(+Ps, +Term) is semidet.
%
% Syntactic sugar for invoking APIs.

'=>'(Ps, Term) :-
    Term =.. [Name|Args],
    TermWithPs =.. [Name|[Ps|Args]],
    ModuledTerm =.. [:, kubepl, TermWithPs],
    call(ModuledTerm).
