:- module(format_test, [ stream_position_at_offset/3 ]).
/** <module> LSP Formatter test

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(prolog_source)).
:- use_module(library(readutil), [ read_line_to_codes/2,
                                   read_file_to_codes/3 ]).
:- use_module(library(dcg/basics), [ whites//1 ]).

:- dynamic foo/1.
:- dynamic(bar/2).

stream_position_at_offset(LineCharMap, To, EndPos) :-
    A = [1, 2, 3, 4],
    _Foo = 'Foo',
    _Bar = "Bar",
    CharCount = To,
    ByteCount = To, % need to check for multibyte...
    Neckish = x,
    memberchk(Neckish, [':-', '=>', '-->']),
    findall(X, ( member(X, A),
                 0 is X mod 2
               ),
            _),
    findall(X,
            ( member(X, A),
              0 is X mod 2 ),
            _
    ),
    file_offset_line_position(LineCharMap, To, LineCount, LinePosition),
    EndPos = '$stream_position_data'(CharCount, LineCount, LinePosition, ByteCount).

beep(A), [X] -->
    { X = beep },
    { Z = beep,
      quux(Z), _W = [   110, 110, 32   ] },
    string_without(`foo`, A).

emit_reified_(To, term_begin(_, _, Func, _, Parens)) =>
    ( is_operator(Func)
    -> format(To, "~w", [Func])
    ;    format(To, "~q", [Func]) ),
    ( Parens = true
    -> format(To, "(", [])
    ; true ).

/*
weird_quasi(Quasi) :-
    Quasi = {|html(X)||<html>{{X}}</html>|}.
*/

burf :-
    A = _{
            a: 1, b: [x, "y", 'Z'|Tail], 'C': x{x: 1, b: 2}
    },
    write([1, 2|_]), write([1, 2|[x]]),
    B = foo{q: q},
    C = Something{x: y},
    Tail = [gurf],
    write(A.b),
    Something = aoeu,
    format("~q", C),
    display(B).

eval_20(Eq, RetType, Depth, Self, [V|VI], VVO):-  \+ is_list(VI), !,
    eval_args(Eq, RetType, Depth, Self, VI, VM),
    ( VM\==VI -> eval_args(Eq, RetType, Depth, Self, [V|VM], VVO) ;
          (eval_args(Eq, RetType, Depth, Self, V, VV), (V\==VV -> eval_args(Eq, RetType, Depth, Self, [VV|VI], VVO) ; VVO = [V|VI])) ).

foo(A) :-
    findall(X, (
                member(X, A),

                0 is X mod 2
               ),
            _).

bar(A) :-
    findall(X, (
                member(X, A),
                0 is X mod 2
               ),
            _).

bas(A) :-
    findall(X, ( member(X, A),
                 0 is X mod 2
               ),
            _).

baz(A) :-
    findall(X,
            ( member(X, A),
              0 is X mod 2
            ),
            _).

whitespace_indentation_for_state(State, Indent) :-
    get_dict(state, State, Stack),
    aggregate_all(count,
                  ( member(X, Stack),
                    memberchk(X, [parens_begin, braces_begin, term_begin(_, _, _)]) ),
                  ParensCount),
    Indent is ParensCount * 2 + _ToplevelIndent.

%aoeu
hello(A) :-
    ( A
    -> gurf
    ; burf
    ).

send_message(Stream, Msg) :-
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes), width(0)]),
    phrase(utf8_codes(JsonCodes), UTF8Codes),
    length(UTF8Codes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]),
    flush_output(Stream).

aoeuoaeuoeau(A) :-
    A = [0'a, 0xa, 1e-3, 0.001].

expand_subterm_positions(Term, _TermState, term_position(_From, _To, FFrom, FTo, SubPoses),
                         Expanded, ExTail), functor(Term, ',', _, _) =>
    % special-case comma terms to be reified as commas
    Expanded = [comma(FFrom, FTo)|ExpandedTail0], % aligned
    functor(Term, _, Arity, _),                   % comments
    expand_term_subterms_positions(false, Term, Arity, 1, SubPoses, ExpandedTail0, ExTail).

foo(A, B, C, D, E) :-
    ( A = 1
    -> B = 2
    ; ( C = 3
      -> D = 4
      ; E = 5 ) ).

testing_dict_formatting(A) :-
    findall(B,
            ( Foo = _{x: 1,
                      y: 2,
                      c: 3
              },
              Bar = Foo.y,
              between(0, Bar, B)
            ),
            A).

sthsnthsnth(X) :-
    X = ':'(_, _).

% end comment
