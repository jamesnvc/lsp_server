:- module(test, []).

% привет мир
foo(X) :-
    X = 'значение',
    writeln(X).
