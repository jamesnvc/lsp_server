:- module(lsp_reading_source, [ file_lines_start_end/2,
                                read_term_positions/2,
                                file_offset_line_position/4 ]).
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
                                                      syntax_errors(error)]),
          maplist([Name=Var]>>( Var = '$var'(Name) ), VarNames),
          arg(1, Acc, Lst),
          nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                               varible_names: VarNames, comments: Comments}|Lst]),
          Term = end_of_file, !
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).

file_offset_line_position(LineCharMap-_, CharCount, Line, LinePosition) :-
    ground(CharCount), !,
    rb_lookup_range(CharCount, Start-_End, Line, LineCharMap),
    LinePosition #= CharCount - Start.
file_offset_line_position(_-LineCharMap, CharCount, Line, LinePosition) :-
    rb_lookup(Line, Start-_End, LineCharMap),
    CharCount #= Start + LinePosition.

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
