:- module(lsp_reading_source, [ file_lines_start_end/2,
                                read_term_positions/2,
                                read_term_positions/4,
                                file_offset_line_position/4,
                                find_in_term_with_positions/5,
                                position_to_match/3,
                                subterm_leaf_position/4
                              ]).
/** <module> LSP Reading Source

Module for reading in Prolog source code with positions, mostly
wrapping prolog_read_source_term/4.

@author James Cash

@tbd Files using quasi-quotations currently aren't supported; need to
     teach prolog_read_source_term/4 to load correctly

*/

:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(clpfd)).
:- use_module(library(prolog_source)).
:- use_module(library(readutil), [ read_line_to_codes/2 ]).
:- use_module(library(yall)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specialized reading predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! file_lines_start_end(+Path:text, -LineCharRange:list) is det.
%
%  Construct a mapping of file offsets to line numbers in the file at
%  Path. LineCharRange will be a list containing terms like
%  =line_start_end(LineNumber, LineOffsetStart, LineOffsetEnd)=
file_lines_start_end(Path, LineCharRange) :-
    Acc = line_data([], line(1, 0)),
    setup_call_cleanup(
        open(Path, read, Stream),
        ( repeat,
          read_line_to_codes(Stream, Line),
          stream_property(Stream, position(Position)),
          stream_position_data(char_count, Position, NewLineStart),
          arg(2, Acc, line(LastLine, LastLineStart)),
          arg(1, Acc, Data),
          LastLineEnd is NewLineStart - 1,
          nb_setarg(1, Acc, [(LastLineStart-LastLineEnd)-LastLine|Data]),
          NextLine is LastLine + 1,
          nb_setarg(2, Acc, line(NextLine, NewLineStart)),
          Line == end_of_file, !
        ),
        close(Stream)),
    arg(1, Acc, Ranges),
    list_to_rbtree(Ranges, RangeToLine),
    maplist([Range-Line, Line-Range]>>true, Ranges, InvRanges),
    list_to_rbtree(InvRanges, LineToRange),
    LineCharRange = RangeToLine-LineToRange.

%! read_term_positions(+Path:text, -TermsWithPositions:list) is det.
%
%  Read in all the terms in the file at Path, using
%  prolog_read_source_term/4, to a list of dictionaries.
%  Each dictionary has the following keys:
%    * term
%      The term read in, with variables replace with the term '$var'(VariableName).
%    * pos
%      The position of the term (see [[prolog_read_source_term/4]]).
%    * subterm
%      The position of the subterms in term (see [[prolog_read_source_term/4]]).
%    * variable_names
%      List of Name=Var terms for the variables in Term. Note that the
%      variables in term have already been replace with var(Name)
%    * comments
%      Comments in the term, with the same format as prolog_read_source_term/4
read_term_positions(Path, TermsWithPositions) :-
    Acc = data([]),
    prolog_canonical_source(Path, SourceId),
    setup_call_cleanup(
        prolog_open_source(SourceId, Stream),
        ( repeat,
          prolog_read_source_term(Stream, Term, _Ex, [term_position(TermPos),
                                                      subterm_positions(SubTermPos),
                                                      variable_names(VarNames),
                                                      comments(Comments),
                                                      % maybe use `error` for running standalone?
                                                      syntax_errors(dec10)]),
          maplist([Name=Var]>>( Var = '$var'(Name) ), VarNames),
          arg(1, Acc, Lst),
          nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                               varible_names: VarNames, comments: Comments}|Lst]),
          Term = end_of_file, !
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).

%! read_term_positions(+Path:text, +Start:integer, +End:integer, -TermsWithPositions:list) is det.
%
%  Read in all the terms in the file at Path between Start and End, using
%  prolog_read_source_term/4, to a list of dictionaries.
%  Each dictionary has the following keys:
%    * term
%      The term read in, with variables replace with the term '$var'(VariableName).
%    * pos
%      The position of the term (see [[prolog_read_source_term/4]]).
%    * subterm
%      The position of the subterms in term (see [[prolog_read_source_term/4]]).
%    * variable_names
%      List of Name=Var terms for the variables in Term. Note that the
%      variables in term have already been replace with var(Name)
%    * comments
%      Comments in the term, with the same format as prolog_read_source_term/4
read_term_positions(Path, Start, End, TermsWithPositions) :-
    Acc = data([]),
    prolog_canonical_source(Path, SourceId),
    setup_call_cleanup(
        prolog_open_source(SourceId, Stream),
        ( repeat,
          prolog_read_source_term(Stream, Term, _Ex, [term_position(TermPos),
                                                      subterm_positions(SubTermPos),
                                                      variable_names(VarNames),
                                                      comments(Comments),
                                                      % maybe use `error` for running standalone?
                                                      syntax_errors(dec10)]),
          arg(1, SubTermPos, TermStart),
          TermStart >= Start,
          maplist([Name=Var]>>( Var = '$var'(Name) ), VarNames),
          arg(1, Acc, Lst),
          nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                               varible_names: VarNames, comments: Comments}|Lst]),
          arg(2, SubTermPos, TermEnd),
          once(( Term = end_of_file ; TermEnd >= End )), !
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Using LineCharMap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! file_offset_line_position(+LineCharMap:term, ?Offset:integer, ?Line:integer, ?Column:integer) is semidet.
%
%  True when LineCharMap is a term as created by
%  file_lines_start_end/2, Offset is the offset into the file, Line is
%  the line number and Column is the character within that line.
%
%  Presumably either Offset is ground or Line & Column are.
file_offset_line_position(LineCharMap-_, CharCount, Line, LinePosition) :-
    ground(CharCount), !,
    rb_lookup_range(CharCount, Start-_End, Line, LineCharMap),
    LinePosition #= CharCount - Start.
file_offset_line_position(_-LineCharMap, CharCount, Line, LinePosition) :-
    rb_lookup(Line, Start-_End, LineCharMap),
    CharCount #= Start + LinePosition.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Red-black trees helper
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rb_lookup_range(Key, KeyRange, Value, t(_, Tree)) =>
    rb_lookup_range_(Key, KeyRange, Value, Tree).

rb_lookup_range_(_Key, _KeyRange, _Value, black('', _, _, '')) :- !, fail.
rb_lookup_range_(Key, KeyRange, Value, Tree) :-
    arg(2, Tree, Start-End),
    compare(CmpS, Key, Start),
    compare(CmpE, Key, End),
    rb_lookup_range_(t(CmpS, CmpE), Key, Start-End, KeyRange, Value, Tree).

rb_lookup_range_(t(>, <), _, Start-End, KeyRange, Value, Tree) =>
    arg(3, Tree, Value),
    KeyRange = Start-End.
rb_lookup_range_(t(=, _), _, Start-End, KeyRange, Value, Tree) =>
    arg(3, Tree, Value),
    KeyRange = Start-End.
rb_lookup_range_(t(_, =), _, Start-End, KeyRange, Value, Tree) =>
    arg(3, Tree, Value),
    KeyRange = Start-End.
rb_lookup_range_(t(<, _), Key, _, KeyRange, Value, Tree) =>
    arg(1, Tree, NTree),
    rb_lookup_range_(Key, KeyRange, Value, NTree).
rb_lookup_range_(t(_, >), Key, _, KeyRange, Value, Tree) =>
    arg(4, Tree, NTree),
    rb_lookup_range_(Key, KeyRange, Value, NTree).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Searching through read results
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

position_to_match(LineCharRange, found_at(_, From-To), Match) :- !,
    file_offset_line_position(LineCharRange, From, FromLine1, FromCharacter),
    file_offset_line_position(LineCharRange, To, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.
position_to_match(LineCharRange, found_at(_, term_position(_, _, FFrom, FTo, _)), Match) :-
    file_offset_line_position(LineCharRange, FFrom, FromLine1, FromCharacter),
    file_offset_line_position(LineCharRange, FTo, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.

:- meta_predicate find_in_term_with_positions(1, +, +, -, -).

%! find_in_term_with_positions(+Search:callable, +Term, +Positions, -Matches, -Tail) is det.
%
%  True when Search is a callable that takes a term as argument and is
%  true if the term should be included in match, Term is the term in
%  which to search, Positions is the subterm positions as given from read_term_positions/2,
%  Matches is a list of the found matching terms, and Tail is the tail of the Matches list.
find_in_term_with_positions(Needle, Term, Position, Matches, Tail) :-
    call(Needle, Term), !, % recurse?
    Matches = [found_at(Term, Position)|Tail].
find_in_term_with_positions(Needle, Term, term_position(_, _, _, _, SubPoses), Matches, Tail) :- !,
    find_in_term_subterm(Needle, Term, 1, SubPoses, Matches, Tail).
find_in_term_with_positions(Needle, Term, list_position(_, _, Elms, TailPos), Matches, Tail) :- !,
    find_in_term_list(Needle, Term, Elms, TailPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, brace_term_position(_, _, ArgPos), Matches, Tail) :- !,
    Term = {Term0},
    find_in_term_with_positions(Needle, Term0, ArgPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, parentheses_term_position(_, _, ContentPos), Matches, Tail) :- !,
    find_in_term_with_positions(Needle, Term, ContentPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, dict_position(_, _, _, _, ContentPos), Matches, Tail) :- !,
    find_in_term_dict(Needle, Term, ContentPos, Matches, Tail).
find_in_term_with_positions(_, _Term, _Pos, Tail, Tail).

find_in_term_dict(_, _, [], Tail, Tail) :- !.
find_in_term_dict(Needle, Term, [Pos|Poses], Matches, Tail) :-
    key_value_position(_KVFrom, _KVTo, _SF, _ST, Key, _KeyPos, ValuePos) = Pos,
    get_dict(Key, Term, Value),
    find_in_term_with_positions(Needle, Value, ValuePos, Matches, Tail0),
    find_in_term_dict(Needle, Term, Poses, Tail0, Tail).

find_in_term_list(_, _, [], none, Tail, Tail) :- !.
find_in_term_list(Needle, TailElt, [], TailPos, Matches, Tail) :- !,
    find_in_term_with_positions(Needle, TailElt, TailPos, Matches, Tail).
find_in_term_list(Needle, [X|Xs], [Pos|Poses], TailPos, Matches, Tail) :-
    find_in_term_with_positions(Needle, X, Pos, Matches, Tail0),
    find_in_term_list(Needle, Xs, Poses, TailPos, Tail0, Tail).

find_in_term_subterm(_, _, _, [], Tail, Tail) :- !.
find_in_term_subterm(Needle, Term, Arg, [Position|Positions], Matches, Tail) :-
    arg(Arg, Term, SubTerm),
    NextArg is Arg + 1,
    find_in_term_with_positions(Needle, SubTerm, Position, Matches, Matches0),
    find_in_term_subterm(Needle, Term, NextArg, Positions, Matches0, Tail).

%! subterm_leaf_position(+Term, +Offset, +SubTermPoses, ?Leaf) is semidet.
subterm_leaf_position(Term, Offset, From-To, Term) :- between(From, To, Offset), !.
subterm_leaf_position(Term, Offset, term_position(_, _, FFrom, FTo, _), Term) :-
    between(FFrom, FTo, Offset), !.
subterm_leaf_position(Term, Offset, term_position(From, To, _, _, Subterms), Leaf) :-
    between(From, To, Offset), !,
    functor(Term, _, Arity, _),
    between(1, Arity, Arg),
    arg(Arg, Term, Subterm),
    nth1(Arg, Subterms, SubtermPos),
    subterm_leaf_position(Subterm, Offset, SubtermPos, Leaf), !.
subterm_leaf_position(Term, Offset, list_position(From, To, Elms, _), Leaf) :-
    between(From, To, Offset),
    length(Elms, NElms),
    between(1, NElms, Idx),
    nth1(Idx, Term, Elm),
    nth1(Idx, Elms, ElmPos),
    subterm_leaf_position(Elm, Offset, ElmPos, Leaf), !.
subterm_leaf_position(Term, Offset, list_position(From, To, Elms, TailPos), Leaf) :-
    between(From, To, Offset), TailPos \= none, !,
    length(Elms, NElms),
    length(Head, NElms),
    append(Head, Tail, Term),
    subterm_leaf_position(Tail, Offset, TailPos, Leaf), !.
subterm_leaf_position(Term, Offset, brace_term_position(From, To, BracesPos), Leaf) :-
    between(From, To, Offset), !,
    Term = {Term0},
    subterm_leaf_position(Term0, Offset, BracesPos, Leaf).
subterm_leaf_position(Term, Offset, parentheses_term_position(From, To, ContentPos), Leaf) :-
    between(From, To, Offset), !,
    subterm_leaf_position(Term, Offset, ContentPos, Leaf).
subterm_leaf_position(Term, Offset, dict_position(_From, _To, TagFrom, TagTo, _KVPoses), Leaf) :-
    between(TagFrom, TagTo, Offset), !,
    is_dict(Term, Leaf).
subterm_leaf_position(Term, Offset, dict_position(From, To, _TagFrom, _TagTo, KVPoses), Leaf) :-
    between(From, To, Offset), !,
    member(key_value_position(KVFrom, KVTo, _SF, _ST, Key, _KeyPos, ValuePos), KVPoses),
    between(KVFrom, KVTo, Offset), !,
    % keys of a literal dict aren't of interest, I think?
    get_dict(Key, Term, Value),
    subterm_leaf_position(Value, Offset, ValuePos, Leaf).
