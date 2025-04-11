:- module(utils_input1, []).

:- use_module(library(apply)).

xhighlights_at_position(Path, line_char(Line1, Char0), Leaf, Highlights) :-
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
    xposition_to_match(LineCharRange), % shouldn't match
    subterm_leaf_position(TermInfo.term, Offset, SubTermPoses, Leaf),
    ( Leaf = '$var'(_)
      % if it's a variable, only look inside the containing term
    -> find_occurrences_of_var(Leaf, TermInfo, Matches),
       xposition_to_match(LineCharRange, _, _) % another match
    % if it's the functor of a term, find all occurrences in the file
    ; functor(Leaf, FuncName, Arity),
      find_occurrences_of_func(FuncName, Arity, TermsWithPositions, Matches)
    ),
    maplist(xposition_to_match(LineCharRange), Matches, Highlights).

xposition_to_match(LineCharRange, found_at(_, From-To), Match) :- !,
    file_offset_line_position(LineCharRange, From, FromLine1, FromCharacter),
    file_offset_line_position(LineCharRange, To, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.
xposition_to_match(LineCharRange, found_at(_, term_position(_, _, FFrom, FTo, _)), Match) :-
    file_offset_line_position(LineCharRange, FFrom, FromLine1, FromCharacter),
    file_offset_line_position(LineCharRange, FTo, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.
