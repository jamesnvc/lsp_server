:- module(utils, [called_at/3,
                  name_callable/2,
                  relative_ref_location/4,
                  help_at_position/5,
                  clause_in_file_at_position/3
                 ]).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source), [read_source_term_at_location/3]).
:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).

:- if(current_predicate(xref_called/5)).
called_at(Path, Callable, By-local(Line)) :-
    xref_called(Path, Callable, By, _, Line).
:- else.
called_at(Path, Callable, By-Ref) :-
    xref_called(Path, Callable, By),
    xref_defined(Path, By, Ref).
:- endif.

name_callable(Name/0, Name) :- !.
name_callable(Name/Arity, Callable) :-
    length(FakeArgs, Arity),
    Callable =.. [Name|FakeArgs], !.

relative_ref_location(Here, _, local(Line1),
                      _{uri: Here, range: _{start: _{line: Line0, character: 1},
                                            end: _{line: NextLine, character: 0}}}) :-
    !, succ(Line0, Line1), succ(Line1, NextLine).
relative_ref_location(_, Goal, imported(Path), Location) :-
    atom_concat('file://', Path, ThereUri),
    xref_source(Path),
    xref_defined(Path, Goal, Loc),
    relative_ref_location(ThereUri, Goal, Loc, Location).

help_at_position(Path, Line1, Char0, Id, _{id: Id, result: _{contents: S}}) :-
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    predicate_help(Path, Clause, S), !.
help_at_position(_, _, _, Id, _{id: Id, result: null}).

% [TODO] use xref_comment to get docs for local predicates
predicate_help(_, Pred, Help) :-
    nonvar(Pred),
    help_objects(Pred, exact, Matches), !,
    catch(help_html(Matches, exact-exact, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).

linechar_offset(Stream, line_char(Line1, Char1), Offset) :-
    seek(Stream, 0, bof, _),
    seek_to_line(Stream, Line1),
    seek(Stream, Char1, current, Offset).

seek_to_line(Stream, N) :-
    N > 1, !,
    skip(Stream, 0'\n),
    NN is N - 1,
    seek_to_line(Stream, NN).
seek_to_line(_, _).

clause_in_file_at_position(Clause, Path, Position) :-
    setup_call_cleanup(
        open(Path, read, Stream, []),
        clause_at_position(Stream, Clause, Position),
        close(Stream)
    ).

clause_at_position(Stream, Clause, Start) :-
    linechar_offset(Stream, Start, Offset), !,
    clause_at_position(Stream, Clause, Start, Offset).
clause_at_position(Stream, Clause, line_char(Line1, Char), Here) :-
    read_source_term_at_location(Stream, Terms, [line(Line1),
                                                 subterm_positions(SubPos),
                                                 error(Error)]),
    extract_clause_at_position(Stream, Terms, line_char(Line1, Char), Here,
                               SubPos, Error, Clause).

extract_clause_at_position(Stream, _, line_char(Line1, Char), Here, _, Error, Clause) :-
    nonvar(Error), !, Line1 > 1,
    LineBack is Line1 - 1,
    clause_at_position(Stream, Clause, line_char(LineBack, Char), Here).
extract_clause_at_position(_, Terms, _, Here, SubPos, _, Clause) :-
    find_clause(Terms, Here, SubPos, Clause).

find_clause(Term, Offset, F-T, Term/0) :-
    between(F, T, Offset), !,
    atom(Term).
find_clause(Term, Offset, term_position(_, _, FF, FT, _), Name/Arity) :-
    between(FF, FT, Offset), !,
    functor(Term, Name, Arity).
find_clause(Term, Offset, term_position(F, T, _, _, SubPoses), Clause) :-
    between(F, T, Offset), !,
    Term =.. [_|SubTerms],
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos), !,
    find_clause(SubTerm, Offset, SubPos, Clause).
find_clause(Term, Offset, term_position(F, T, _, _, _), Name/Arity) :-
    between(F, T, Offset), !,
    functor(Term, Name, Arity).
find_clause(Term, Offset, parentheses_term_position(F, T, SubPoses), Clause) :-
    between(F, T, Offset),
    find_clause(Term, Offset, SubPoses, Clause).
find_clause({SubTerm}, Offset, brace_term_position(F, T, SubPos), Clause) :-
    between(F, T, Offset),
    find_clause(SubTerm, Offset, SubPos, Clause).

find_containing_term(Offset, [Term|_], [F-T|_], Term, F-T) :-
    between(F, T, Offset).
find_containing_term(Offset, [Term|_], [P|_], Term, P) :-
    P = term_position(F, T, _, _, _),
    between(F, T, Offset), !.
find_containing_term(Offset, [Term|_], [PP|_], Term, P) :-
    PP = parentheses_term_position(F, T, P),
    between(F, T, Offset), !.
find_containing_term(Offset, [BTerm|_], [BP|_], Term, P) :-
    BP = brace_term_position(F, T, P),
    {Term} = BTerm,
    between(F, T, Offset).
find_containing_term(Offset, [_|Ts], [_|Ps], T, P) :-
    find_containing_term(Offset, Ts, Ps, T, P).
