:- module(lsp_formatter, []).
/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(prolog_source)).
:- use_module(library(readutil), [read_line_to_codes/2]).

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
          nb_setarg(1, Acc, [line_start_end(LastLine, LastLineStart, LastLineEnd)|Data]),
          NextLine is LastLine + 1,
          nb_setarg(2, Acc, line(NextLine, NewLineStart)),
          Line == end_of_file, !
        ),
        close(Stream)),
    arg(1, Acc, RangesReversed),
    reverse(RangesReversed, LineCharRange).

read_term_positions(Path, TermsWithPositions) :-
    Acc = data([]),
    prolog_canonical_source(Path, SourceId),
    setup_call_cleanup(
        prolog_open_source(SourceId, Stream),
        ( repeat,
          prolog_read_source_term(Stream, Term, _Ex, [term_position(TermPos),
                                                      subterm_positions(SubTermPos),
                                                      variable_names(VarNames),
                                                      comments(Comments)]),
          ( Term \= end_of_file
          -> arg(1, Acc, Lst),
             nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                                  varible_names: VarNames, comments: Comments}|Lst]),
             fail
          ; ! )
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).

reified_format_for_file(Path, Reified) :-
    read_term_positions(Path, TermsWithPos),
    file_lines_start_end(Path, LinesStartEnd),
    InitState = _{last_line: 1, last_char: 0, line_bounds: LinesStartEnd},
    expand_term_positions(InitState, TermsWithPos, Reified).

expand_term_positions(_, [], []).
expand_term_positions(State0, [InfoDict|Rest], Expanded0) :-
    % use byte count of comment position vs term position to decide which to output first?
    % flatten out the comments and terms?
    ( InfoDict.comments \= []
    -> expand_comments_positions(State0, State, InfoDict.comments, Expanded0, Expanded)
    ;  ( Expanded = Expanded0, State = State0, Expanded = Expanded0 ) ),

    TermPos = InfoDict.pos,
    sync_position_whitespace(State, TermPos, Expanded, Expanded1),
    Expanded1 = [term(InfoDict.term)|Expanded2],

    subterm_end_position(State.line_bounds, InfoDict.subterm, TermEndPos),

    update_state_position(State, TermEndPos, State1),
    expand_term_positions(State1, Rest, Expanded2).

expand_comments_positions(State, State, [], Tail, Tail) :- !.
expand_comments_positions(State0, State, [Comment|Rest], Expanded, Tail) :-
    expand_comment_positions(State0, State1, Comment, Expanded, Tail0),
    expand_comments_positions(State1, State, Rest, Tail0, Tail).

expand_comment_positions(State0, State1, CommentPos-Comment, Expanded, ExpandedTail) :-
    sync_position_whitespace(State0, CommentPos, Expanded, Expanded1),
    Expanded1 = [comment(Comment)|ExpandedTail],
    term_end_position(Comment, CommentEndPosRel),
    increment_stream_position(CommentPos, CommentEndPosRel, CommentEndPos),
    update_state_position(State0, CommentEndPos, State1).

increment_stream_position(StartPos, RelPos, EndPos) :-
    stream_position_data(char_count, StartPos, StartCharCount),
    stream_position_data(char_count, RelPos, RelCharCount),
    CharCount is StartCharCount + RelCharCount,
    stream_position_data(byte_count, StartPos, StartByteCount),
    stream_position_data(byte_count, RelPos, RelByteCount),
    ByteCount is StartByteCount + RelByteCount,
    stream_position_data(line_count, StartPos, StartLineCount),
    stream_position_data(line_count, RelPos, RelLineCount),
    stream_position_data(line_position, StartPos, StartLinePosition),
    stream_position_data(line_position, RelPos, RelLinePosition),
    ( RelLineCount == 1
    -> LineCount = StartLineCount,
       LinePosition is StartLinePosition + RelLinePosition
    ; ( LineCount is StartLineCount + RelLineCount - 1,
        LinePosition = RelLinePosition )),
    EndPos = '$stream_position_data'(CharCount, LineCount, LinePosition, ByteCount).

update_state_position(State0, EndPos, State2) :-
    stream_position_data(line_count, EndPos, EndLineCount),
    stream_position_data(line_position, EndPos, EndLinePos),
    put_dict(last_line, State0, EndLineCount, State1),
    put_dict(last_char, State1, EndLinePos, State2).

sync_position_whitespace(State, TermPos, Expanded, ExpandedTail) :-
    PrevLineCount = State.last_line,
    stream_position_data(line_count, TermPos, NewLineCount),
    NewLines is NewLineCount - PrevLineCount,
    ( NewLines > 0
    -> n_copies_of(NewLines, newline, Expanded, Expanded0),
       PrevLinePosition = 0
    ;  ( Expanded = Expanded0,
         PrevLinePosition = State.last_char )
    ),

    stream_position_data(line_position, TermPos, NewLinePosition),
    Whitespace is NewLinePosition - PrevLinePosition,
    ( Whitespace > 0
    -> Expanded0 = [white(Whitespace)|ExpandedTail]
    ;  Expanded0 = ExpandedTail ).

file_offset_line_position(LineCharMap, CharCount, Line, LinePosition) :-
    member(line_start_end(Line, Start, End), LineCharMap),
    between(Start, End, CharCount), !,
    LinePosition is CharCount - Start.

subterm_end_position(LineCharMap, term_position(_From, To, _FFrom, _FTo, _), EndPos) =>
    % TODO probably do need to split out the functor & the whole thing...somewhere
    % breaking the rules, building an opaque term
    CharCount = To,
    ByteCount = To, % need to check for multibyte...
    file_offset_line_position(LineCharMap, To, LineCount, LinePosition),
    EndPos = '$stream_position_data'(CharCount, LineCount, LinePosition, ByteCount).
subterm_end_position(LineCharMap, _From-To, EndPos) =>
    CharCount = To,
    ByteCount = To, % need to check for multibyte...
    file_offset_line_position(LineCharMap, To, LineCount, LinePosition),
    EndPos = '$stream_position_data'(CharCount, LineCount, LinePosition, ByteCount).

term_end_position(Term, Position) :-
    setup_call_cleanup(
        open_null_stream(Out),
        ( write(Out, Term),
          stream_property(Out, position(Position))
        ),
        close(Out)).

n_copies_of(0, _, Tail, Tail) :- !.
n_copies_of(N, ToCopy, [ToCopy|Rest], Tail) :-
    N1 is N - 1,
    n_copies_of(N1, ToCopy, Rest, Tail).
