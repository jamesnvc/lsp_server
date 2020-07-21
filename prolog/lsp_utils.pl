:- module(lsp_utils, [called_at/4,
                      defined_at/3,
                      name_callable/2,
                      relative_ref_location/4,
                      help_at_position/4,
                      clause_in_file_at_position/3,
                      clause_variable_positions/3,
                      seek_to_line/2,
                      linechar_offset/3
                     ]).
/** <module> LSP Utils

Module with a bunch of helper predicates for looking through prolog
source and stuff.

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(apply), [maplist/3, exclude/3]).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source), [read_source_term_at_location/3]).
:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).
:- use_module(library(solution_sequences), [distinct/2]).
:- use_module(library(lists), [append/3, member/2, selectchk/4]).
:- use_module(library(sgml), [load_html/3]).

:- if(current_predicate(xref_called/5)).
%! called_at(+Path:atom, +Clause:term, -By:term, -Location:term) is nondet.
%  Find the callers and locations of the goal =Clause=, starting from
%  the file =Path=. =Location= will be bound to all the callers and
%  locations that the =Clause= is called from like =Caller-Location=.
%
%  @see find_subclause/4
called_at(Path, Clause, By, Location) :-
    name_callable(Clause, Callable),
    xref_source(Path),
    xref_called(Path, Callable, By, _, CallerLine),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        ( find_subclause(Stream, Clause, CallerLine, Locations),
          member(Location, Locations),
          Location \= position(_, 0) ),
        close(Stream)
    ).
called_at(Path, Name/Arity, By, Location) :-
    DcgArity is Arity + 2,
    name_callable(Name/DcgArity, Callable),
    xref_source(Path),
    xref_called(Path, Callable, By, _, CallerLine),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        ( find_subclause(Stream, Name/Arity, CallerLine, Locations),
          member(Location, Locations),
          Location \= position(_, 0) ),
        close(Stream)
    ).
:- else.
called_at(Path, Callable, By, Ref) :-
    xref_called(Path, Callable, By),
    xref_defined(Path, By, Ref).
:- endif.

defined_at(Path, Name/Arity, Location) :-
    name_callable(Name/Arity, Callable),
    xref_source(Path),
    xref_defined(Path, Callable, Ref),
    atom_concat('file://', Path, Doc),
    relative_ref_location(Doc, Callable, Ref, Location).
defined_at(Path, Name/Arity, Location) :-
    % maybe it's a DCG?
    DcgArity is Arity + 2,
    name_callable(Name/DcgArity, Callable),
    xref_source(Path),
    xref_defined(Path, Callable, Ref),
    atom_concat('file://', Path, Doc),
    relative_ref_location(Doc, Callable, Ref, Location).


find_subclause(Stream, Subclause, CallerLine, Locations) :-
    read_source_term_at_location(Stream, Term, [line(CallerLine),
                                                subterm_positions(Poses)]),
    findall(Offset, distinct(Offset, find_clause(Term, Offset, Poses, Subclause)),
            Offsets),
    collapse_adjacent(Offsets, StartOffsets),
    maplist(offset_line_char(Stream), StartOffsets, Locations).

offset_line_char(Stream, Offset, position(Line, Char)) :-
    % seek(Stream, 0, bof, _),
    % for some reason, seek/4 isn't zeroing stream line position
    set_stream_position(Stream, '$stream_position'(0,0,0,0)),
    setup_call_cleanup(
        open_null_stream(NullStream),
        copy_stream_data(Stream, NullStream, Offset),
        close(NullStream)
    ),
    stream_property(Stream, position(Pos)),
    stream_position_data(line_count, Pos, Line),
    stream_position_data(line_position, Pos, Char).

collapse_adjacent([X|Rst], [X|CRst]) :-
    collapse_adjacent(X, Rst, CRst).
collapse_adjacent(X, [Y|Rst], CRst) :-
    succ(X, Y), !,
    collapse_adjacent(Y, Rst, CRst).
collapse_adjacent(_, [X|Rst], [X|CRst]) :- !,
    collapse_adjacent(X, Rst, CRst).
collapse_adjacent(_, [], []).


%! name_callable(?Name:functor, ?Callable:term) is det.
%  True when, if Name = Func/Arity, Callable = Func(_, _, ...) with
%  =Arity= args.
name_callable(Name/0, Name) :- atom(Name), !.
name_callable(Name/Arity, Callable) :-
    length(FakeArgs, Arity),
    Callable =.. [Name|FakeArgs], !.

%! relative_ref_location(+Path:atom, +Goal:term, +Position:position(int, int), -Location:dict) is semidet.
%  Given =Goal= found in =Path= and position =Position= (from
%  called_at/3), =Location= is a dictionary suitable for sending as an
%  LSP response indicating the position in a file of =Goal=.
relative_ref_location(Here, _, position(Line0, Char1),
                      _{uri: Here, range: _{start: _{line: Line0, character: Char1},
                                            end: _{line: Line1, character: 0}}}) :-
    !, succ(Line0, Line1).
relative_ref_location(Here, _, local(Line1),
                      _{uri: Here, range: _{start: _{line: Line0, character: 1},
                                            end: _{line: NextLine, character: 0}}}) :-
    !, succ(Line0, Line1), succ(Line1, NextLine).
relative_ref_location(_, Goal, imported(Path), Location) :-
    atom_concat('file://', Path, ThereUri),
    xref_source(Path),
    xref_defined(Path, Goal, Loc),
    relative_ref_location(ThereUri, Goal, Loc, Location).

%! help_at_position(+Path:atom, +Line:integer, +Char:integer, -Help:string) is det.
%
%  =Help= is the documentation for the term under the cursor at line
%  =Line=, character =Char= in the file =Path=.
help_at_position(Path, Line1, Char0, S) :-
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    predicate_help(Path, Clause, S0),
    format_help(S0, S).

%! format_help(+Help0, -Help1) is det.
%
%  Reformat help string, so the first line is the signature of the predicate.
format_help(HelpFull, Help) :-
    split_string(HelpFull, "\n", " ", Lines0),
    exclude([Line]>>string_concat("Availability: ", _, Line),
            Lines0, Lines1),
    exclude([""]>>true, Lines1, Lines2),
    Lines2 = [HelpShort|_],
    split_string(HelpFull, "\n", "", HelpLines),
    selectchk(HelpShort, HelpLines, "", HelpLines0),
    append([HelpShort], HelpLines0, HelpLines1),
    atomic_list_concat(HelpLines1, "\n", Help).

predicate_help(_, Pred, Help) :-
    nonvar(Pred),
    help_objects(Pred, exact, Matches), !,
    catch(help_html(Matches, exact-exact, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).
predicate_help(HerePath, Pred, Help) :-
    xref_source(HerePath),
    name_callable(Pred, Callable),
    xref_defined(HerePath, Callable, Loc),
    location_path(HerePath, Loc, Path),
    once(xref_comment(Path, Callable, Summary, Comment)),
    pldoc_process:parse_comment(Comment, Path:0, Parsed),
    memberchk(mode(Signature, Mode), Parsed),
    memberchk(predicate(_, Summary, _), Parsed),
    format(string(Help), "  ~w is ~w.~n~n~w", [Signature, Mode, Summary]).
predicate_help(_, Pred/_Arity, Help) :-
    help_objects(Pred, dwim, Matches), !,
    catch(help_html(Matches, dwim-Pred, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).

location_path(HerePath, local(_), HerePath).
location_path(_, imported(Path), Path).

linechar_offset(Stream, line_char(Line1, Char0), Offset) :-
    seek(Stream, 0, bof, _),
    seek_to_line(Stream, Line1),
    seek(Stream, Char0, current, Offset).

seek_to_line(Stream, N) :-
    N > 1, !,
    skip(Stream, 0'\n),
    NN is N - 1,
    seek_to_line(Stream, NN).
seek_to_line(_, _).

clause_variable_positions(Path, Line, Variables) :-
    xref_source(Path),
    findall(Op, xref_op(Path, Op), Ops),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        ( read_source_term_at_location(Stream, Term,
                                 [line(Line),
                                  subterm_positions(SubPos),
                                  variable_names(VarNames),
                                  operators(Ops),
                                  error(_Error)]),
          findall(
              VarName-Locations,
              ( member(VarName=Var, VarNames),
                findall(Offset, find_var(Term, Offset, SubPos, Var), Offsets),
                collapse_adjacent(Offsets, ColOffsets),
                maplist(offset_line_char(Stream), ColOffsets, Locations)
              ),
              Variables)
        ),
        close(Stream)
    ).

clause_in_file_at_position(Clause, Path, Position) :-
    xref_source(Path),
    findall(Op, xref_op(Path, Op), Ops),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        clause_at_position(Stream, Ops, Clause, Position),
        close(Stream)
    ).

clause_at_position(Stream, Ops, Clause, Start) :-
    linechar_offset(Stream, Start, Offset), !,
    clause_at_position(Stream, Ops, Clause, Start, Offset).
clause_at_position(Stream, Ops, Clause, line_char(Line1, Char), Here) :-
    read_source_term_at_location(Stream, Terms, [line(Line1),
                                                 subterm_positions(SubPos),
                                                 operators(Ops),
                                                 error(Error)]),
    extract_clause_at_position(Stream, Ops, Terms, line_char(Line1, Char), Here,
                               SubPos, Error, Clause).

extract_clause_at_position(Stream, Ops, _, line_char(Line1, Char), Here, _,
                           Error, Clause) :-
    nonvar(Error), !, Line1 > 1,
    LineBack is Line1 - 1,
    clause_at_position(Stream, Ops, Clause, line_char(LineBack, Char), Here).
extract_clause_at_position(_, _, Terms, _, Here, SubPos, _, Clause) :-
    once(find_clause(Terms, Here, SubPos, Clause)).

%! find_clause(+Term:term, ?Offset:int, +Position:position, ?Subclause) is nondet.
%  True when =Subclause= is a subclause of =Term= at offset =Offset=
%  and =Position= is the term positions for =Term= as given by
%  read_term/3 with =subterm_positions(Position)=.
find_clause(Term, Offset, F-T, Clause) :-
    between(F, T, Offset),
    ground(Term), Clause = Term/0.
find_clause(Term, Offset, term_position(_, _, FF, FT, _), Name/Arity) :-
    between(FF, FT, Offset),
    functor(Term, Name, Arity).
find_clause(Term, Offset, term_position(F, T, _, _, SubPoses), Clause) :-
    between(F, T, Offset),
    Term =.. [_|SubTerms],
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos),
    find_clause(SubTerm, Offset, SubPos, Clause).
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
find_containing_term(Offset, [Terms|_], [LP|_], Term, P) :-
    LP = list_position(_F, _T, Ps, _),
    find_containing_term(Offset, Terms, Ps, Term, P).
find_containing_term(Offset, [Dict|_], [DP|_], Term, P) :-
    DP = dict_position(_, _, _, _, Ps),
    member(key_value_position(_F, _T, _SepF, _SepT, Key, _KeyPos, ValuePos),
          Ps),
    get_dict(Key, Dict, Value),
    find_containing_term(Offset, [Value], [ValuePos], Term, P).
find_containing_term(Offset, [_|Ts], [_|Ps], T, P) :-
    find_containing_term(Offset, Ts, Ps, T, P).

find_var(Term, Offset, F-T, Var) :-
    Var == Term,
    between(F, T, Offset).
find_var(Term, Offset, term_position(F, T, _, _, SubPoses), Var) :-
    between(F, T, Offset),
    Term =.. [_|SubTerms],
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos),
    find_var(SubTerm, Offset, SubPos, Var).
find_var(Term, Offset, parentheses_term_position(F, T, SubPoses), Var) :-
    between(F, T, Offset),
    find_var(Term, Offset, SubPoses, Var).
find_var({SubTerm}, Offset, brace_term_position(F, T, SubPos), Var) :-
    between(F, T, Offset),
    find_var(SubTerm, Offset, SubPos, Var).
