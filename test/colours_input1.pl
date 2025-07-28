:- module(test, []).

:- use_module(library(apply), [
    maplist/2, % foo bar baz
	maplist/3, % foo bar baz
    foldl/4
]).

foo(X) :-
    Foo = 1,
	maplist(is_list, [[aaaaaaaa], 1, _]), % foo bar
    % aoeu aoeu snth
    maplist(succ, [1,3,3], X).
