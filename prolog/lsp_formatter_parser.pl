:- module(lsp_formatter_parser, [ reified_format_for_file/2,
                                  emit_reified/2 ]).
/** <module> LSP Parser For Formatter

Module for parsing Prolog source code, for subsequent formatting

@author James Cash

@tbd Files using quasi-quotations currently aren't supported; need to
     teach prolog_read_source_term/4 to load correctly

*/

:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(clpfd)).
:- use_module(library(prolog_source)).
:- use_module(library(readutil), [ read_line_to_codes/2,
                                   read_file_to_string/3 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reading in terms
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
          nb_setarg(1, Acc, [line_start_end(LastLine, LastLineStart, LastLineEnd)|Data]),
          NextLine is LastLine + 1,
          nb_setarg(2, Acc, line(NextLine, NewLineStart)),
          Line == end_of_file, !
        ),
        close(Stream)),
    arg(1, Acc, RangesReversed),
    reverse(RangesReversed, LineCharRange).

%! read_term_positions(+Path:text, -TermsWithPositions:list) is det.
%
%  Read in all the terms in the file at Path, using
%  prolog_read_source_term/4, to a list of dictionaries.
%  Each dictionary has the following keys:
%    * term
%      The term read in, with variables replace with the term var(VariableName).
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
          maplist([Name=Var]>>( Var = var(Name) ), VarNames),
          arg(1, Acc, Lst),
          nb_setarg(1, Acc, [_{term: Term, pos: TermPos, subterm: SubTermPos,
                               varible_names: VarNames, comments: Comments}|Lst]),
          Term = end_of_file, !
        ),
        prolog_close_source(Stream)),
    arg(1, Acc, TermsWithPositionsRev),
    reverse(TermsWithPositionsRev, TermsWithPositions).

:- thread_local current_source_string/1.

%! reified_format_for_file(+Path:string, -Reified:list) is det.
%
%  Read the prolog source file at Path into a flattened list of terms
%  indicating content, comments, and whitespace.
reified_format_for_file(Path, Reified) :-
    retractall(current_source_string(_)),
    read_file_to_string(Path, FileString, []),
    read_term_positions(Path, TermsWithPos),
    setup_call_cleanup(
        assertz(current_source_string(FileString)),
        expand_term_positions(TermsWithPos, Reified0),
        retractall(current_source_string(_))
    ),
    sort(1, @=<, Reified0, Reified1),
    file_lines_start_end(Path, LinesStartEnd),
    InitState = _{last_line: 1, last_char: 0, line_bounds: LinesStartEnd},
    add_whitespace_terms(InitState, Reified1, Reified2),
    simplify_reified_terms(Reified2, Reified).

% Remove no-longer needed positioning information to make things less
% annoying for later steps.
simplify_reified_terms(In, Out) :-
    maplist(simplify_reified_term, In, Out).

simplify_reified_term(newline, newline) :- !.
simplify_reified_term(white(N), white(N)) :- !.
simplify_reified_term(Term, SimpleTerm) :-
    % all other terms have two extra args, From & To
    compound_name_arguments(Term, Name, [_, _|Args]),
    ( Args = []
    -> SimpleTerm = Name
    ;  compound_name_arguments(SimpleTerm, Name, Args) ).

%! emit_reified(+To, +Reified) is det.
%
%  Output source file as read with reified_format_for_file/2 to To, as
%  format/3.
emit_reified(_, []) :- !.
emit_reified(To, [Term|Rest]) :-
    emit_reified_(To, Term),
    emit_reified(To, Rest).

emit_reified_(To, newline) => format(To, "~n", []).
emit_reified_(To, white(N)) =>
    length(Whites, N),
    maplist(=(0' ), Whites),
    format(To, "~s", [Whites]).
emit_reified_(To, comma) => format(To, ",", []).
emit_reified_(To, simple(T)) =>
    format(To, "~s", [T]).
emit_reified_(To, simple_quoted(T)) =>
    format(To, "'~q'", [T]).
emit_reified_(To, string(T)), string(T) =>
    format(To, "~q", [T]).
emit_reified_(To, string(T)) =>
    % string term, but not a string, must be codes
    format(To, "`~s`", [T]).
emit_reified_(To, term_begin(Func, _, Parens)) =>
    ( Parens = true
    -> Format = "~q("
    ;  Format = "~w" ),
    format(To, Format, [Func]).
emit_reified_(To, term_end(Parens, TermState)) =>
    ( Parens = true
    -> MaybeClose = ")"
    ; MaybeClose = "" ),
    ( TermState = toplevel
    -> MaybeStop = "."
    ; MaybeStop = "" ),
    format(To, "~w~w", [MaybeClose, MaybeStop]).
emit_reified_(To, list_begin) =>
    format(To, "[", []).
emit_reified_(To, list_tail) =>
    format(To, "|", []).
emit_reified_(To, list_end) =>
    format(To, "]", []).
emit_reified_(To, comment(Text)) =>
    format(To, "~s", [Text]).
emit_reified_(To, braces_begin) =>
    format(To, "{", []).
emit_reified_(To, braces_end) =>
    format(To, "}", []).
emit_reified_(To, parens_begin) =>
    format(To, "(", []).
emit_reified_(To, parens_end) =>
    format(To, ")", []).
emit_reified_(To, dict_tag(var(Tag))) =>
    format(To, "~w", [Tag]).
emit_reified_(To, dict_tag(Tag)), var(Tag) =>
    % if Tag is still a var, it must be anonymous
    format(To, "_", []).
emit_reified_(To, dict_tag(Tag)) =>
    % if Tag is still a var, it must be anonymous
    format(To, "~w", [Tag]).
emit_reified_(To, dict_begin) =>
    format(To, "{", []).
emit_reified_(To, dict_sep) =>
    format(To, ":", []).
emit_reified_(To, dict_end) =>
    format(To, "}", []).

%! add_whitespace_terms(+State:dict, +Reified:list, -Formatted:list) is det.
%
%  Add terms indicating whitespace and newlines in between positioned
%  terms, as created by reified_format_for_file/2.
add_whitespace_terms(_State, [], [newline]) :- !.
add_whitespace_terms(State, [Term|Terms], Out) :-
    arg(1, Term, TermStart),
    stream_position_at_offset(State.line_bounds, TermStart, Pos),
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
    ( Term \= end_of_file % just for comments at the end
    -> expand_subterm_positions(Term, toplevel, InfoDict.subterm,
                                Expanded1, Expanded2)
    ;  Expanded2 = Expanded1 ),

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

expand_subterm_positions(Term, _TermState, term_position(_From, _To, FFrom, FTo, SubPoses),
                         Expanded, ExTail), functor(Term, ',', _, _) =>
    % special-case comma terms to be reified as commas
    Expanded = [comma(FFrom, FTo)|ExpandedTail0],
    functor(Term, _, Arity, _),
    expand_term_subterms_positions(false, Term, Arity, 1, SubPoses, ExpandedTail0, ExTail).
expand_subterm_positions(Term, TermState, term_position(From, To, FFrom, FTo, SubPoses),
                         Expanded, ExTail) =>
    % using functor/4 to allow round-tripping zero-arity functors
    functor(Term, Func, Arity, TermType),
    % better way to tell if term is parenthesized?
    % read functor from current_source_string/1 (as with simple below)
    % and see if parens are there?
    (  From = FFrom, max_subterm_to(SubPoses, SubTermMax), To > SubTermMax
    -> ( Parens = true, FTo1 is FTo + 1 ) % add space for the parenthesis
    ;  ( Parens = false, FTo1 = FTo )  ),
    Expanded = [term_begin(FFrom, FTo1, Func, TermType, Parens)|ExpandedTail0],
    expand_term_subterms_positions(Parens, Term, Arity, 1, SubPoses,
                                   ExpandedTail0, ExpandedTail1),
    succ(To0, To),
    ExpandedTail1 = [term_end(To0, To, Parens, TermState)|ExpandedTail2],
    maybe_add_comma(TermState, To, ExpandedTail2, ExTail).
expand_subterm_positions(Term, TermState, string_position(From, To), Expanded, Tail) =>
    Expanded = [string(From, To, Term)|Tail0],
    maybe_add_comma(TermState, To, Tail0, Tail).
expand_subterm_positions(_Term, TermState, From-To, Expanded, Tail) =>
    current_source_string(FileString),
    Length is To - From,
    sub_string(FileString, From, Length, _, SimpleString),
    Expanded = [simple(From, To, SimpleString)|Tail0],
    maybe_add_comma(TermState, To, Tail0, Tail).
expand_subterm_positions(Term, TermState, list_position(From, To, Elms, HasTail), Expanded, Tail) =>
    assertion(is_listish(Term)),
    ListBeginTo is From + 1,
    Expanded = [list_begin(From, ListBeginTo)|Expanded1],
    expand_list_subterms_positions(Term, Elms, Expanded1, Expanded2),
    succ(To0, To),
    (  HasTail = none
    -> Expanded2 = [list_end(To0, To)|Tail0]
    ;  ( arg(1, HasTail, TailFrom),
         succ(TailBarFrom, TailFrom),
         Expanded2 = [list_tail(TailBarFrom, TailFrom)|Expanded3],
         list_tail(Term, Elms, ListTail),
         expand_subterm_positions(ListTail, false, HasTail, Expanded3, Expanded4),
         Expanded4 = [list_end(To0, To)|Tail0] )  ),
    maybe_add_comma(TermState, To, Tail0, Tail).
expand_subterm_positions(Term, TermState, brace_term_position(From, To, BracesPos), Expanded, Tail) =>
    BraceTo is From + 1,
    Expanded = [braces_begin(From, BraceTo)|Tail0],
    Term = {Term0},
    expand_subterm_positions(Term0, false, BracesPos, Tail0, Tail1),
    succ(To1, To),
    Tail1 = [braces_end(To1, To)|Tail2],
    maybe_add_comma(TermState, To1, Tail2, Tail).
expand_subterm_positions(Term, TermState, parentheses_term_position(From, To, ContentPos),
                         Expanded, Tail) =>
    ParenTo is From + 1,
    Expanded = [parens_begin(From, ParenTo)|Tail0],
    expand_subterm_positions(Term, false, ContentPos, Tail0, Tail1),
    succ(To1, To),
    Tail1 = [parens_end(To1, To)|Tail2],
    maybe_add_comma(TermState, To, Tail2, Tail).
expand_subterm_positions(Term, TermState, dict_position(_From, To, TagFrom, TagTo, KeyValPos),
                         Expanded, Tail) =>
    is_dict(Term, Tag),
    DictBraceTo is TagTo + 1,
    Expanded = [dict_tag(TagFrom, TagTo, Tag), dict_begin(TagTo, DictBraceTo)|Tail0],
    expand_dict_kvs_positions(Term, KeyValPos, Tail0, Tail1),
    succ(To1, To),
    Tail1 = [dict_end(To1, To)|Tail2],
    maybe_add_comma(TermState, To, Tail2, Tail).

maybe_add_comma(subterm_item, CommaFrom, Tail0, Tail) :- !,
    CommaTo is CommaFrom + 1,
    Tail0 = [comma(CommaFrom, CommaTo)|Tail].
maybe_add_comma(_, _, Tail, Tail).

is_listish(L) :- \+ var(L), !.
is_listish([]).
is_listish([_|_]).

list_tail(Tail, [], Tail) :- !.
list_tail([_|Rest], [_|PosRest], Tail) :-
    list_tail(Rest, PosRest, Tail).

max_subterm_to(SubPoses, SubTermMaxTo) :-
    aggregate_all(max(To),
                  ( member(Pos, SubPoses),
                    arg(2, Pos, To) ),
                  SubTermMaxTo).

expand_dict_kvs_positions(_, [], Tail, Tail) :- !.
expand_dict_kvs_positions(Dict, [Pos|Poses], Expanded0, Tail) :-
    Pos = key_value_position(_From, To, SepFrom, SepTo, Key, KeyPos, ValuePos),
    get_dict(Key, Dict, Value),
    expand_subterm_positions(Key, false, KeyPos, Expanded0, Expanded1),
    Expanded1 = [dict_sep(SepFrom, SepTo)|Expanded2],
    expand_subterm_positions(Value, false, ValuePos, Expanded2, Expanded3),
    CommaTo is To + 1,
    ( Poses = [_|_]
    -> Expanded3 = [comma(To, CommaTo)|Expanded4]
    ;  Expanded3 = Expanded4 ),
    expand_dict_kvs_positions(Dict, Poses, Expanded4, Tail).

% possible for the list to still have a tail when out of positions
expand_list_subterms_positions(_, [], Tail, Tail) :- !.
expand_list_subterms_positions([Term|Terms], [Pos|Poses], Expanded, Tail) :-
    ( Poses = [_|_]
    -> TermState = subterm_item
    ;  TermState = false ),
    expand_subterm_positions(Term, TermState, Pos, Expanded, Expanded1),
    expand_list_subterms_positions(Terms, Poses, Expanded1, Tail).

expand_term_subterms_positions(_Parens, _Term, _Arity, _Arg, [], Tail, Tail) :- !.
expand_term_subterms_positions(Parens, Term, Arity, Arg, [SubPos|Poses], Expanded, ExpandedTail) :-
    assertion(between(1, Arity, Arg)),
    arg(Arg, Term, SubTerm),
    ( Parens = true, Arg < Arity
    -> State = subterm_item
    ;  State = false ),
    expand_subterm_positions(SubTerm, State, SubPos, Expanded, Expanded0),
    succ(Arg, Arg1),
    expand_term_subterms_positions(Parens, Term, Arity, Arg1, Poses, Expanded0, ExpandedTail).

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
        LinePosition = RelLinePosition ) ),
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
    between(Start, End, CharCount),
    LinePosition #= CharCount - Start, !.

stream_position_at_offset(LineCharMap, To, EndPos) :-
    CharCount = To,
    ByteCount = To, % need to check for multibyte...
    file_offset_line_position(LineCharMap, To, LineCount, LinePosition),
    % breaking the rules, building an opaque term
    EndPos = '$stream_position_data'(CharCount, LineCount, LinePosition, ByteCount).

% Helpers

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
