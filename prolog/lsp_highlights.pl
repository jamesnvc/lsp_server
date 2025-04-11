:- module(lsp_highlights, [ highlights_at_position/3 ]).

:- include('_lsp_path_add.pl').

:- use_module(library(apply), [maplist/2]).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).

:- use_module(lsp(lsp_reading_source)).

highlights_at_position(Path, Position, Highlights) :-
    highlights_at_position(Path, Position, _, Highlights).

highlights_at_position(Path, line_char(Line1, Char0), Leaf, Highlights) :-
    file_lines_start_end(Path, LineCharRange),
    read_term_positions(Path, TermsWithPositions),
    % find the top-level term that the offset falls within
    file_offset_line_position(LineCharRange, Offset, Line1, Char0),
    % find the specific sub-term containing the point
    member(TermInfo, TermsWithPositions),
    SubTermPoses = TermInfo.subterm,
    arg(1, SubTermPoses, TermFrom),
    arg(2, SubTermPoses, TermTo),
    between(TermFrom, TermTo, Offset), !,
    subterm_leaf_position(TermInfo.term, Offset, SubTermPoses, Leaf),
    ( Leaf = '$var'(_)
      % if it's a variable, only look inside the containing term
    -> find_occurrences_of_var(Leaf, TermInfo, Matches)
    % if it's the functor of a term, find all occurrences in the file
    ; functor(Leaf, FuncName, Arity),
      find_occurrences_of_func(FuncName, Arity, TermsWithPositions, Matches)
    ),
    maplist(position_to_match(LineCharRange), Matches, Highlights).

find_occurrences_of_func(FuncName, Arity, TermInfos, Matches) :-
    find_occurrences_of_func(FuncName, Arity, TermInfos, Matches, []).
find_occurrences_of_func(_, _, [], Tail, Tail).
find_occurrences_of_func(FuncName, Arity, [TermInfo|Rest], Matches, Tail) :-
    find_in_term_with_positions({FuncName, Arity}/[X, _]>>( nonvar(X),
                                                            functor(X, FuncName, Arity) ),
                                TermInfo.term, TermInfo.subterm, Matches, Tail0),
    find_occurrences_of_func(FuncName, Arity, Rest, Tail0, Tail).

find_occurrences_of_var(Var, TermInfo, Matches) :-
    Var = '$var'(Name), ground(Name), % wrapped term; otherwise it's anonymous & matches nothing
    Term = TermInfo.term,
    Poses = TermInfo.subterm,
    find_in_term_with_positions({Var}/[X, _]>>( ground(X), X = Var ), Term, Poses,
                                Matches, []).
