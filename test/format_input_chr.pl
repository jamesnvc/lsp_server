:- module(foo, []).

:- use_module(library(chr)).

kept(X) ==> guard(X) | first(X), second(X).
remove(X) <=> body(X).
keep(X) \ remove(X) <=> guard(X) | body(X).
