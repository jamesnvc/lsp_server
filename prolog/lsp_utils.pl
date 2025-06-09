:- module(lsp_utils, [called_at/3,
                      defined_at/3,
                      name_callable/2,
                      relative_ref_location/4,
                      help_at_position/4,
                      clause_in_file_at_position/3,
                      clause_variable_positions/3,
                      clause_import_position/4,
                      seek_to_line/2,
                      linechar_offset/3,
                      url_path/2,
                      unlimited//1
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
:- use_module(library(yall)).
:- use_module(library(debug)).

:- include('_lsp_path_add.pl').

:- use_module(lsp(lsp_reading_source), [ file_lines_start_end/2,
                                         read_term_positions/2,
                                         read_term_positions/4,
                                         find_in_term_with_positions/5,
                                         position_to_match/3,
                                         file_offset_line_position/4 ]).

:- if(current_predicate(xref_called/5)).
%! called_at(+Path:atom, +Clause:term, -Locations:list) is det.
%  Find the callers and locations of the goal =Clause=, starting from
%  the file =Path=. =Locations= will be a list of all the callers and
%  locations that the =Clause= is called from as LSP-formatted dicts.
called_at(Path, Clause, Locations) :-
    setof(L, Path^Clause^Locs^(
                 called_at_(Path, Clause, Locs),
                 member(L, Locs)
             ),
          Locations), !.
called_at(Path, Clause, Locations) :-
    name_callable(Clause, Callable),
    xref_source(Path),
    xref_called(Path, Callable, _By, _, CallerLine),
    % we couldn't find the definition, but we know it's in that form, so give that at least
    succ(CallerLine0, CallerLine),
    Locations = [_{range: _{start: _{line: CallerLine0, character: 0},
                            end: _{line: CallerLine, character: 0}}}].

called_at_(Path, Clause, Locations) :-
    name_callable(Clause, Callable),
    xref_source(Path),
    xref_called(Path, Callable, _By, _, CallerLine),
    file_lines_start_end(Path, LineCharRange),
    file_offset_line_position(LineCharRange, Offset, CallerLine, 0),
    read_term_positions(Path, Offset, Offset, TermInfos),
    Clause = FuncName/Arity,
    find_occurences_of_callable(Path, FuncName, Arity, TermInfos, Matches, []),
    maplist(position_to_match(LineCharRange), Matches, Locations).
called_at_(Path, Clause, Locations) :-
    xref_source(Path),
    Clause = FuncName/Arity,
    DcgArity is Arity + 2,
    DcgClause = FuncName/DcgArity,
    name_callable(DcgClause, DcgCallable),
    xref_defined(Path, DcgCallable, dcg),
    name_callable(DcgClause, DcgCallable),
    xref_called(Path, DcgCallable, _By, _, CallerLine),
    file_lines_start_end(Path, LineCharRange),
    file_offset_line_position(LineCharRange, Offset, CallerLine, 0),
    read_term_positions(Path, Offset, Offset, TermInfos),
    find_occurences_of_callable(Path, FuncName, DcgArity, TermInfos, Matches, Tail0),
    % also look for original arity in a dcg context
    % TODO: modify this to check that it's inside a DCG if it has this
    % arity...but not in braces?
    find_occurences_of_callable(Path, FuncName, Arity, TermInfos, Tail0, []),
    maplist(position_to_match(LineCharRange), Matches, Locations).
:- else.
called_at(Path, Callable, By, Ref) :-
    xref_called(Path, Callable, By),
    xref_defined(Path, By, Ref).
:- endif.

find_occurences_of_callable(_, _, _, [], Tail, Tail).
find_occurences_of_callable(Path, FuncName, Arity, [TermInfo|TermInfos], Matches, Tail) :-
    FindState = in_meta(false),
    find_in_term_with_positions(term_matches_callable(FindState, Path, FuncName, Arity),
                                TermInfo.term, TermInfo.subterm, Matches, Tail0),
    find_occurences_of_callable(Path, FuncName, Arity, TermInfos, Tail0, Tail).

term_matches_callable(FindState, Path, FuncName, Arity, Term, Position) :-
    arg(1, Position, Start),
    arg(2, Position, End),
    ( arg(1, FindState, in_meta(_, MStart, MEnd)),
      once( Start > MEnd ; End < MStart )
    -> nb_setarg(1, FindState, false)
    ; true ),
    term_matches_callable_(FindState, Path, FuncName, Arity, Term, Position).

term_matches_callable_(_, _, FuncName, Arity, Term, _) :-
    nonvar(Term), Term = FuncName/Arity.
term_matches_callable_(_, _, FuncName, Arity, Term, _) :-
    nonvar(Term),
    functor(T, FuncName, Arity),
    Term = T, !.
term_matches_callable_(State, _, FuncName, Arity, Term, _) :-
    nonvar(Term),
    % TODO check the argument
    arg(1, State, in_meta(N, _, _)),
    MArity is Arity - N,
    functor(T, FuncName, MArity),
    Term = T, !.
term_matches_callable_(State, Path, _, _, Term, Position) :-
    nonvar(Term), compound(Term),
    compound_name_arity(Term, ThisName, ThisArity),
    name_callable(ThisName/ThisArity, Callable),
    xref_meta(Path, Callable, Called),
    member(E, Called), nonvar(E), E = _+N, integer(N),
    arg(1, Position, Start),
    arg(2, Position, End),
    nb_setarg(1, State, in_meta(N, Start, End)),
    fail.

%! url_path(?FileUrl:atom, ?Path:atom) is det.
%
%  Convert between file:// url and path
url_path(Url, Path) :-
    current_prolog_flag(windows, true),
    % on windows, in neovim at least, textDocument URI looks like
    % "file:///C:/foo/bar/baz.pl"; we need to strip off another
    % leading slash to get a valid path
    atom_concat('file:///', Path, Url), !.
url_path(Url, Path) :-
    atom_concat('file://', Path, Url).

defined_at(Path, Name/Arity, Location) :-
    name_callable(Name/Arity, Callable),
    xref_source(Path),
    xref_defined(Path, Callable, Ref),
    url_path(Doc, Path),
    relative_ref_location(Doc, Callable, Ref, Location).
defined_at(Path, Name/Arity, Location) :-
    % maybe it's a DCG?
    DcgArity is Arity + 2,
    name_callable(Name/DcgArity, Callable),
    xref_source(Path),
    xref_defined(Path, Callable, Ref),
    url_path(Doc, Path),
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
    set_stream_position(Stream, '$stream_position'(0, 0, 0, 0)),
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
    url_path(ThereUri, Path),
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
    exclude(=(""), Lines1, Lines2),
    Lines2 = [HelpShort|_],
    split_string(HelpFull, "\n", " ", HelpLines),
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
/*
predicate_help(_, Pred/_Arity, Help) :-
    help_objects(Pred, dwim, Matches), !,
    catch(help_html(Matches, dwim-Pred, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).
*/

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
    file_lines_start_end(Path, LineCharRange),
    read_term_positions(Path, TermsWithPositions),
    % find the top-level term that the offset falls within
    file_offset_line_position(LineCharRange, Offset, Line, 0),
    member(TermInfo, TermsWithPositions),
    SubTermPoses = TermInfo.subterm,
    arg(1, SubTermPoses, TermFrom),
    arg(2, SubTermPoses, TermTo),
    between(TermFrom, TermTo, Offset), !,
    find_in_term_with_positions(
        [X, _]>>( \+ \+ ( X = '$var'(Name), ground(Name) ) ),
        TermInfo.term,
        TermInfo.subterm,
        VariablesPositions, []
    ),
    findall(
        VarName-Locations,
        group_by(
            VarName,
            Location,
            ( member(found_at('$var'(VarName), Location0-_), VariablesPositions),
              file_offset_line_position(LineCharRange, Location0, L1, C),
              succ(L0, L1),
              Location = position(L0, C)
            ),
            Locations
        ),
        Variables).

clause_import_position(Path, Line, Pattern, Position) :-
    file_lines_start_end(Path, LineCharRange),
    read_term_positions(Path, TermsWithPositions),
    % find the top-level term that the offset falls within
    file_offset_line_position(LineCharRange, Offset, Line, 0),
    member(TermInfo, TermsWithPositions),
    SubTermPoses = TermInfo.subterm,
    arg(1, SubTermPoses, TermFrom),
    arg(2, SubTermPoses, TermTo),
    between(TermFrom, TermTo, Offset), !,
    find_in_term_with_positions(
        {Pattern}/[Term, _]>>(
            ( Term = ( :- use_module(Pattern) ) -> true
            ; Term = ( :- use_module(Pattern, _) )
            )
        ),
        TermInfo.term,
        TermInfo.subterm,
        Positions,
        []
    ),
    member(
        found_at(
            _Term,
            term_position(_, _, _, _, [ % :- ...
                term_position(_, _, _, _, [ % use_module(...)
                    SpecPos | _Rest
                ]) 
            ])
        ),
        Positions
    ),
    termpos_start_end(SpecPos, Start, End),

        %     [found_at((:-use_module(library(qwerty), [x/1])),
        %     term_position(232, 269, 232, 234, [ % :- ...
        %         term_position(235, 269, 235, 245, [ % use_module(...)
        %             term_position(246, 261, 246, 253, [254-260]), % library(qwerty)
        %             list_position(263, 268, [
        %                 term_position(264, 267, 265, 266, [264-265, 266-267]) % x/1
        %             ], none)
        %         ])
        %     ]))
        % ])

    % member(found_at(use_module(_58), _70352-_70354),
    % [found_at(use_module(derive), term_position(61, 79, 61, 71, [72-78]))])

    %file_offset_line_position(LineCharRange, Location0, L1, C),
    %succ(L0, L1),
    %Position = position(L0, C).
    file_offset_line_position(LineCharRange, Start, StartLine1, StartCol),
    file_offset_line_position(LineCharRange, End, EndLine1, EndCol),
    succ(StartLine, StartLine1),
    succ(EndLine, EndLine1),
    Position = _{start: _{line: StartLine, character: StartCol},
                 end:   _{line: EndLine,   character: EndCol}}.

termpos_start_end(From-To, From, To) :- !. % Primitive types (atoms, numbers, variables)
termpos_start_end(Term, From, To) :-
    arg(1, Term, From),
    arg(2, Term, To).
    
termpos_start_end(list_position(From, To, _Elems, _Tail), From, To).

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

:- meta_predicate unlimited(//, *, *).

%! unlimited(:Nonterminal)// is semidet.
%
% Tries to parse =Nonterminal= an unlimited number of times. If the provided
% nonterminal ever fails, =unlimited= fails too. This rule can never actually
% succeed (i.e. yield =true=), although it could be considered to succeed "at
% infinity." =Nonterminal= is likely to be a DCG rule which performs side
% effects as it parses.
unlimited(Nonterminal) -->
    call(Nonterminal),
    unlimited(Nonterminal).

