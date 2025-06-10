:- use_module(library(does_not_exist)).
:- use_module(nexiste_pas, [pred/2]).

foo(A) :-
    Gae,
    A = 1,
    _{a: B},
    [1,2,3|Z].

bar :- aoeu snth.
