:- module(foo, []).

muendetIn(trave, ostsee).
muendetIn(stepenitz, trave).
mündetIn(maurine, stepenitz).
muendetIn(elbeLübeckKanal, trave).

fliesstNach(X, Y) :- mündetIn(X, Y).
fliesstNach(X, Y) :- muendetIn(X, Z), fliesstNach(Z, Y).
