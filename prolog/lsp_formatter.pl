:- module(lsp_formatter, [ file_format_edits/2,
                           file_formatted/2 ]).

/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(readutil), [ read_file_to_string/3 ]).
:- use_module(library(macros)).

:- include('path_add.pl').
:- use_module(lsp(lsp_formatter_parser), [ reified_format_for_file/2,
                                           emit_reified/2 ]).

file_format_edits(Path, Edits) :-
    read_file_to_string(Path, OrigText, []),
    split_string(OrigText, "\n", "", OrigLines),
    file_formatted(Path, Formatted),
    with_output_to(string(FormattedText),
                   emit_reified(current_output, Formatted)),
    split_string(FormattedText, "\n", "", FormattedLines),
    create_edit_list(OrigLines, FormattedLines, Edits).

file_formatted(Path, Formatted) :-
    reified_format_for_file(Path, Reified),
    apply_format_rules(Reified, Formatted).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Formatting rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_format_rules(Content, Formatted) :-
    phrase(formatter_rules, Content, Formatted).

formatter_rules -->
    collapse_whitespace,
    commas_exactly_one_space,
    correct_indentation(_{state: [toplevel], column: 0, leading_spaces: []}).

collapse_whitespace([], []) :- !.
collapse_whitespace([white(A), white(B)|InRest], [white(AB)|OutRest]) :- !,
    AB is A + B,
    collapse_whitespace(InRest, OutRest).
collapse_whitespace([In|InRest], [In|OutRest]) :-
    collapse_whitespace(InRest, OutRest).

commas_exactly_one_space([], Out) => Out = [].
commas_exactly_one_space([white(_), comma|InRest], Out) =>
    commas_exactly_one_space([comma|InRest], Out).
commas_exactly_one_space([comma, white(_)|InRest], Out), InRest \= [comment(_)|_] =>
    Out = [comma, white(1)|OutRest],
    commas_exactly_one_space(InRest, OutRest).
commas_exactly_one_space([comma, Next|InRest], Out), Next \= white(_), Next \= newline =>
    Out = [comma, white(1), Next|OutRest],
    commas_exactly_one_space(InRest, OutRest).
commas_exactly_one_space([Other|Rest], Out) =>
    Out = [Other|OutRest],
    commas_exactly_one_space(Rest, OutRest).

#define(toplevel_indent, 4).

% TODO: alignment special-case rule for ->;
correct_indentation(_, [], []) :- !.
correct_indentation(State0,
                    [term_begin(Func, Type, Parens)|InRest],
                    [term_begin(Func, Type, Parens)|OutRest]) :-
    indent_state_top(State0, toplevel),
    Func = ':-', !,
    indent_state_push(State0, declaration, State1),
    update_state_column(State1, term_begin(Func, Type, Parens), State2),
    push_state_open_spaces(State2, InRest, State3),
    correct_indentation(State3, InRest, OutRest).
correct_indentation(State0,
                    [term_begin(Func, Type, Parens)|InRest],
                    [term_begin(Func, Type, Parens)|OutRest]) :-
    indent_state_top(State0, toplevel), !,
    update_state_column(State0, term_begin(Func, Type, Parens), State1),
    indent_state_push(State1, defn_head(State1.column, false), State2),
    push_state_open_spaces(State2, InRest, State3),
    correct_indentation(State3, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    indent_state_top(State0, toplevel),
    In = simple(_), !,
    indent_state_push(State0, defn_head_neck, State1),
    update_state_column(State1, In, State2),
    correct_indentation(State2, InRest, OutRest).
correct_indentation(State0,
                    [term_begin(Neckish, T, P)|InRest],
                    [term_begin(Neckish, T, P)|OutRest]) :-
    memberchk(Neckish, [':-', '=>', '-->']),
    indent_state_top(State0, defn_head_neck), !,
    indent_state_pop(State0, State1),
    indent_state_push(State1, defn_body, State2),
    update_state_column(State2, term_begin(Neckish, T, P), State3),
    push_state_open_spaces(State3, InRest, State4),
    correct_indentation(State4, InRest, OutRest).
correct_indentation(State0, [In|InRest], Out) :-
    In = term_begin('->', compound, false),
    indent_state_top(State0, defn_body_indent), !,
    indent_state_pop(State0, State1),
    % if should align with the open paren, not the first term
    indent_state_pop(State1, State2),
    indent_state_top(State2, Top), % Copy the previous top
    indent_state_push(State2, Top, State3),
    whitespace_indentation_for_state(State3, Indent),
    Out = [white(Indent)|OutRest],
    update_state_column(State3, white(Indent), State4),
    correct_indentation(State4, [In|InRest], OutRest).
correct_indentation(State0, [newline|InRest], [newline|Out]) :- !,
    ( indent_state_top(State0, defn_body_indent)
    -> State1 = State0
    ; indent_state_push(State0, defn_body_indent, State1) ),
    update_state_column(State1, newline, State2),
    correct_indentation(State2, InRest, Out).
correct_indentation(State0, [In|InRest], Out) :-
    indent_state_top(State0, defn_body_indent), !,
    ( In = white(_)
    -> correct_indentation(State0, InRest, Out)
    ;  ( indent_state_pop(State0, State1),
         ( indent_state_top(State1, begin(_, _))
           % state top = begin means prev line ended with an open paren
         -> indent_state_pop(State1, StateX),
            % so pop that off and align as if one step "back"
            whitespace_indentation_for_state(StateX, PrevIndent),
            IncPrevIndent is PrevIndent + 4,
            indent_state_push(StateX, align(IncPrevIndent), State2)
         ; State2 = State1 ),
         update_alignment(State2, State3),
         ( ending_term(In)
           % TODO: this needs some more special casing to act the way I'd like
           % (that is, when the ending )/]/} is on its own line)
         -> indent_state_pop(State3, State_),
            pop_state_open_spaces(State3, _, State4),
            push_state_open_spaces(State4, 0, State5),
            whitespace_indentation_for_state(State_, Indent)
         ; ( whitespace_indentation_for_state(State3, Indent),
             State5 = State3 ) ),
         Out = [white(Indent)|OutRest],
         update_state_column(State5, white(Indent), State6),
         correct_indentation(State6, [In|InRest], OutRest) ) ).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    functor(In, Name, _Arity, _Type),
    atom_concat(_, '_begin', Name), !,
    % if we've just begun something...
    update_alignment(State0, State1),
    update_state_column(State1, In, State2),
    indent_state_push(State2, begin(State2.column, In), State3),
    push_state_open_spaces(State3, InRest, State4),
    correct_indentation(State4, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    indent_state_top(State0, defn_head(_, _)),
    In = term_end(_, S), S \= toplevel, !,
    indent_state_pop(State0, State1),
    indent_state_push(State1, defn_head_neck, State2),
    update_state_column(State2, In, State3),
    pop_state_open_spaces(State3, _, State4),
    correct_indentation(State4, InRest, OutRest).
correct_indentation(State0, [In|InRest], Out) :-
    ending_term(In), !,
    indent_state_pop(State0, State1),
    update_state_column(State1, In, State2),
    pop_state_open_spaces(State2, Spaces, State3),
    ( In \= term_end(false, _), In \= term_end(_, toplevel), Spaces > 0
    -> Out = [white(Spaces), In|OutRest]
    ;  Out = [In|OutRest] ),
    correct_indentation(State3, InRest, OutRest).
correct_indentation(State0, [In, NextIn|InRest], Out) :-
    In = white(_),
    ending_term(NextIn), !,
    correct_indentation(State0, [NextIn|InRest], Out).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    memberchk(In, [white(_), newline]), !,
    update_state_column(State0, In, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :- !,
    ( In \= white(_)
    -> update_alignment(State0, State1)
    ; State1 = State0 ),
    update_state_column(State1, In, State2),
    correct_indentation(State2, InRest, OutRest).

ending_term(Term) :-
    functor(Term, Name, _, _),
    atom_concat(_, '_end', Name).

update_alignment(State0, State2) :-
    indent_state_top(State0, begin(Col, _)), !,
    indent_state_pop(State0, State1),
    AlignCol is max(Col, State1.column),
    indent_state_push(State1, align(AlignCol), State2).
update_alignment(State0, State2) :-
    indent_state_top(State0, defn_head(Col, false)), !,
    indent_state_pop(State0, State1),
    AlignCol is max(Col, State1.column),
    indent_state_push(State1, defn_head(AlignCol, true), State2).
update_alignment(State, State).

whitespace_indentation_for_state(State, Indent) :-
    indent_state_top(State, align(Indent)), !.
whitespace_indentation_for_state(State, Indent) :-
    indent_state_top(State, defn_head(Indent, _)), !.
whitespace_indentation_for_state(State, Indent) :-
    get_dict(state, State, Stack),
    aggregate_all(count,
                  ( member(X, Stack),
                    memberchk(X, [parens_begin, braces_begin, term_begin(_, _, _)]) ),
                  ParensCount),
    ( indent_state_contains(State, defn_body)
    -> MoreIndent = #toplevel_indent
    ;  MoreIndent = 0 ),
    Indent is ParensCount * 2 + MoreIndent.

indent_state_top(State, Top) :-
    _{state: [Top|_]} :< State.

indent_state_contains(State, Needle) :-
    _{state: Stack} :< State,
    memberchk(Needle, Stack).

indent_state_push(State0, NewTop, State1) :-
    _{state: Stack} :< State0,
    put_dict(state, State0, [NewTop|Stack], State1).

indent_state_pop(State0, State1) :-
    _{state: [_|Rest]} :< State0,
    put_dict(state, State0, Rest, State1).

update_state_column(State0, newline, State1) :- !,
    put_dict(column, State0, 0, State1).
update_state_column(State0, Term, State1) :-
    emit_reified(string(S), [Term]),
    string_length(S, Len),
    NewCol is State0.column + Len,
    put_dict(column, State0, NewCol, State1).

push_state_open_spaces(State0, Next, State1) :-
    _{leading_spaces: PrevSpaces} :< State0,
    ( Next = [white(N)|_]
    -> put_dict(leading_spaces, State0, [N|PrevSpaces], State1)
    ; put_dict(leading_spaces, State0, [0|PrevSpaces], State1) ).

pop_state_open_spaces(State0, Top, State1) :-
    _{leading_spaces: [Top|Spaces]} :< State0,
    put_dict(leading_spaces, State0, Spaces, State1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create a List of Edits from the Original and Formatted Lines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_edit_list(Orig, Formatted, Edits) :-
    create_edit_list(0, Orig, Formatted, Edits).

create_edit_list(_, [], [], []) :- !.
create_edit_list(LineNum, [Line|Lines], [], [Edit]) :- !,
    length(Lines, NLines),
    EndLine is LineNum + NLines,
    last([Line|Lines], LastLine),
    string_length(LastLine, LastLineLen),
    Edit = _{range: _{start: _{line: LineNum, character: 0},
                      end: _{line: EndLine, character: LastLineLen}},
             newText: ""}.
create_edit_list(LineNum, [], [NewLine|NewLines], [Edit|Edits]) :- !,
    string_length(NewLine, LenLen),
    Edit = _{range: _{start: _{line: LineNum, character: 0},
                      end: _{line: LineNum, character: LenLen}},
             newText: NewLine},
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, [], NewLines, Edits).
create_edit_list(LineNum, [OrigLine|OrigRest], [FormattedLine|FormattedRest], Edits) :-
    (   OrigLine \= FormattedLine  % Only create an edit if the line has changed
    -> string_length(OrigLine, LineLen), %TODO: what should this be?
       Edit = _{range: _{start: _{line: LineNum, character: 0},
                         end: _{line: LineNum, character: LineLen}},
                newText: FormattedLine},
       Edits = [Edit|EditRest]
    ; EditRest = Edits
    ),
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, OrigRest, FormattedRest, EditRest).

% lsp_formatter:file_formatted('/Users/james/Projects/prolog-lsp/prolog/format_test2.pl', Src), lsp_formatter_parser:emit_reified(user_output, Src).

% lsp_formatter:file_formatted('/Users/james/Projects/prolog-lsp/prolog/format_test.pl', Src), setup_call_cleanup(open('/Users/james/tmp/formatted_out.pl', write, S), lsp_formatter_parser:emit_reified(S, Src), close(S)).
