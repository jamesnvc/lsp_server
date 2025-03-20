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
    expand_term_positions(TermsWithPos, Reified0),
    file_lines_start_end(Path, LinesStartEnd),
    InitState = _{last_line: 1, last_char: 0, line_bounds: LinesStartEnd},
    add_whitespace_terms(InitState, Reified0, Reified).

add_whitespace_terms(_State, [], []) :- !.
add_whitespace_terms(State, [Term|Terms], Out) :-
    arg(1, Term, TermStart),
    stream_position_at_offset(State.line_bounds, TermStart, Pos),
    % hm...but after a term start, should (usually?) be an open paren,
    % not a space, similiarly between a term's args, list's items, etc
    sync_position_whitespace(State, Pos, Out, Out1),
    Out1 = [Term|Out2],
    arg(2, Term, TermEnd),
    stream_position_at_offset(State.line_bounds, TermEnd, EndPos),
    update_state_position(State, EndPos, State1),
    add_whitespace_terms(State1, Terms, Out2).

expand_term_positions([], []).
expand_term_positions([InfoDict|Rest], Expanded0) :-
    ( InfoDict.comments \= []
    -> expand_comments_positions(InfoDict.comments, Expanded0, Expanded1)
    ;  Expanded1 = Expanded0 ),

    Term = InfoDict.term,
    expand_subterm_positions(Term, InfoDict.subterm, Expanded1, Expanded2),
    expand_term_positions(Rest, Expanded2).

expand_comments_positions([], Tail, Tail) :- !.
expand_comments_positions([Comment|Rest], Expanded, Tail) :-
    expand_comment_positions(Comment, Expanded, Tail0),
    expand_comments_positions(Rest, Tail0, Tail).

expand_comment_positions(CommentPos-Comment, Expanded, ExpandedTail) :-
    term_end_position(Comment, CommentEndPosRel),
    increment_stream_position(CommentPos, CommentEndPosRel, CommentEndPos),
    stream_position_data(char_count, CommentPos, From),
    stream_position_data(char_count, CommentEndPos, To),
    Expanded = [comment(From, To, Comment)|ExpandedTail].

expand_subterm_positions(Term, term_position(From, To, FFrom, FTo, SubPoses), Expanded, ExTail) =>
    % using functor/4 to allow round-tripping zero-arity functors
    functor(Term, Func, Arity, TermType),
    % way to tell if term is parenthesized?
    ( From \= FFrom
    -> debug(lsp(format), "From(~q) \\= FFrom(~q); Term ~q ", [From, FFrom, Term])
    ; true ),
    Expanded = [term_begin(FFrom, FTo, Func, TermType)|ExpandedTail0],
    expand_term_subterms_positions(Term, Arity, 1, SubPoses,
                                   ExpandedTail0, ExpandedTail1),
    succ(To0, To),
    ExpandedTail1 = [term_end(To0, To)|ExTail].
expand_subterm_positions(Term, From-To, Expanded, Tail) =>
    Expanded = [simple(From, To, Term)|Tail].
expand_subterm_positions(Term, list_position(From, To, Elms, HasTail), Expanded, Tail) =>
    assertion(is_listish(Term)),
    Expanded = [list_begin(From, To)|Expanded1],
    expand_list_subterms_positions(Term, Elms, Expanded1, Expanded2),
    succ(To0, To),
    (  HasTail = none
    -> Expanded2 = [list_end(To0, To)|Tail]
    % need to expand HasTail too?
    ;  Expanded2 = [list_tail(HasTail), list_end|Tail]).

is_listish(L) :- \+ var(L), !.
is_listish([]).
is_listish([_|_]).

expand_list_subterms_positions([], [], Tail, Tail) :- !.
expand_list_subterms_positions([Term|Terms], [Pos|Poses], Expanded, Tail) :-
    expand_subterm_positions(Term, Pos, Expanded, Expanded1),
    expand_list_subterms_positions(Terms, Poses, Expanded1, Tail).

expand_term_subterms_positions(_Term, _Arity, _Arg, [], Tail, Tail) :- !.
expand_term_subterms_positions(Term, Arity, Arg, [SubPos|Poses], Expanded, ExpandedTail) :-
    assertion(between(1, Arity, Arg)),
    arg(Arg, Term, SubTerm),
    expand_subterm_positions(SubTerm, SubPos, Expanded, Expanded0),
    succ(Arg, Arg1),
    expand_term_subterms_positions(Term, Arity, Arg1, Poses, Expanded0, ExpandedTail).

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
    stream_position_at_offset(LineCharMap, To, EndPos).
subterm_end_position(LineCharMap, _From-To, EndPos) =>
    stream_position_at_offset(LineCharMap, To, EndPos).

stream_position_at_offset(LineCharMap, To, EndPos) :-
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
