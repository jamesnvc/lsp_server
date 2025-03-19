:- module(lsp_formatter, []).
/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(prolog_source)).

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
          /*
          debug(lsp(format), "~n~nREAD SOURCE~nTERM: ~q~nExpanded: ~q~nTermPos: ~q~nSubTermPos: ~q~nVarNames: ~q~nComments: ~q~n~n",
                [Term, Ex, TermPos, SubTermPos, VarNames, Comments]),
          */
          arg(1, Acc, Lst),
          nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                               varible_names: VarNames, comments: Comments}|Lst]),
          Term == end_of_file, !
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).

reified_format_for_file(Path, Reified) :-
    read_term_positions(Path, TermsWithPos),
    expand_term_positions(_{last_line: 1, last_char: 0}, TermsWithPos, Reified).

expand_term_positions(_, [], []).
expand_term_positions(State0, [InfoDict|Rest], Expanded0) :-
    % use byte count of comment position vs term position to decide which to output first?
    % flatten out the comments and terms?
    ( InfoDict.comments \= []
    -> expand_comments_positions(State0, State, InfoDict.comments, Expanded0, Expanded)
    ;  Expanded = Expanded0 ),

    TermPos = InfoDict.pos,
    sync_position_whitespace(State, TermPos, Expanded, Expanded1),
    Expanded1 = [term(InfoDict.term)|Expanded2],
    update_state_position(State, TermPos, InfoDict.term, State1),
    expand_term_positions(State1, Rest, Expanded2).

expand_comments_positions(State, State, [], Tail, Tail) :- !.
expand_comments_positions(State0, State, [Comment|Rest], Expanded, Tail) :-
    expand_comment_positions(State0, State1, Comment, Expanded, Tail0),
    expand_comments_positions(State1, State, Rest, Tail0, Tail).

expand_comment_positions(State0, State1, CommentPos-Comment, Expanded, ExpandedTail) :-
    sync_position_whitespace(State0, CommentPos, Expanded, Expanded1),
    Expanded1 = [comment(Comment)|ExpandedTail],
    update_state_position(State0, CommentPos, Comment, State1).

update_state_position(State0, TermPos, Term, State2) :-
    stream_position_data(line_count, TermPos, NewLineCount),
    stream_position_data(line_position, TermPos, NewLinePosition),
    term_end_position(Term, EndPos),
    stream_position_data(line_count, EndPos, EndLineCount),
    stream_position_data(line_position, EndPos, EndLinePos),
    % debug(lsp(format), "term start: ~w ~w end: ~w ~w", [NewLineCount, NewLinePosition, EndLineCount, EndLinePos]),
    ( EndLineCount > 1
    -> NewLineCount1 is NewLineCount + EndLineCount - 1,
       NewLinePosition1 = EndLinePos
    ; ( NewLineCount1 = NewLineCount,
        NewLinePosition1 is NewLinePosition + EndLinePos )
    ),

    put_dict(last_line, State0, NewLineCount1, State1),
    put_dict(last_char, State1, NewLinePosition1, State2).

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
