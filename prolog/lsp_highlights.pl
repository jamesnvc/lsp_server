:- module(lsp_highlights, [ highlights_at_position/3 ]).

:- include('path_add.pl').

:- use_module(library(apply), [maplist/2]).
:- use_module(library(apply_macros)).

:- use_module(lsp(lsp_formatter_parser), []).

highlights_at_position(Path, Position, Highlights) :-
    highlights_at_position(Path, Position, _, Highlights).

highlights_at_position(Path, line_char(Line1, Char0), Leaf, Highlights) :-
    % use the read predicate from formatter_parser + the offset <=> line number mapping
    lsp_formatter_parser:file_lines_start_end(Path, LineCharRange),
    lsp_formatter_parser:read_term_positions(Path, TermsWithPositions),
    % find the top-level term that the offset falls within
    lsp_formatter_parser:file_offset_line_position(LineCharRange, Offset, Line1, Char0),
    % find the specific sub-term containing the point
    member(TermInfo, TermsWithPositions),
    SubTermPoses = TermInfo.subterm,
    arg(1, SubTermPoses, TermFrom),
    arg(2, SubTermPoses, TermTo),
    between(TermFrom, TermTo, Offset), !,
    subterm_leaf_position(TermInfo.term, Offset, SubTermPoses, Leaf),
    % if it's the functor of a term, find all occurrences in the file
    ( Leaf = '$var'(_)
    -> find_occurrences_of_var(Leaf, TermInfo, Matches)
    ; functor(Leaf, FuncName, Arity),
      find_occurrences_of_func(FuncName, Arity, TermsWithPositions, Matches)
    ),
    maplist(position_to_match(LineCharRange), Matches, Highlights).

position_to_match(LineCharRange, From-To, Match) :-
    lsp_formatter_parser:file_offset_line_position(LineCharRange, From, FromLine1, FromCharacter),
    lsp_formatter_parser:file_offset_line_position(LineCharRange, To, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.
position_to_match(LineCharRange, term_position(_, _, FFrom, FTo, _), Match) :-
    lsp_formatter_parser:file_offset_line_position(LineCharRange, FFrom, FromLine1, FromCharacter),
    lsp_formatter_parser:file_offset_line_position(LineCharRange, FTo, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.

find_occurrences_of_func(FuncName, Arity, TermInfos, Matches) :-
    find_occurrences_of_func(FuncName, Arity, TermInfos, Matches, []).

find_occurrences_of_func(_, _, [], Tail, Tail).
find_occurrences_of_func(FuncName, Arity, [TermInfo|Rest], Matches, Tail) :-
    find_in_term_with_positions({FuncName, Arity}/[X]>>( nonvar(X),
                                                         functor(X, FuncName, Arity) ),
                                TermInfo.term, TermInfo.subterm, Matches, Tail0),
    find_occurrences_of_func(FuncName, Arity, Rest, Tail0, Tail).

find_occurrences_of_var(Var, TermInfo, Matches) :-
    Var = '$var'(Name), ground(Name), % wrapped term; otherwise it's anonymous & matches nothing
    Term = TermInfo.term,
    Poses = TermInfo.subterm,
    find_in_term_with_positions({Var}/[X]>>( ground(X), X = Var ), Term, Poses,
                                Matches, []).

:- meta_predicate find_in_term_with_positions(1, +, +, -, -).

find_in_term_with_positions(Needle, Term, Position, Matches, Tail) :-
    call(Needle, Term), !, % recurse?
    Matches = [Position|Tail].
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
find_in_term_list(Needle, TailElt, [], TailPos, Matches, Tail) :-
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

%! subterm_leaf_position(Term, Offset, SubTermPoses, Leaf) is semidet.
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
    between(From, To, Offset), !,
    length(Elms, NElms),
    between(1, NElms, Idx),
    nth1(Idx, Term, Elm),
    nth1(Idx, Elms, ElmPos),
    subterm_leaf_position(Elm, Offset, ElmPos, Leaf), !.
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
