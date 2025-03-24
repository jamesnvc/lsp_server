:- module(lsp_formatter, [ file_format_edits/2 ]).

/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(readutil), [ read_file_to_string/3 ]).
:- use_module(library(macros)).

:- use_module(lsp_formatter_parser, [ reified_format_for_file/2,
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
    commas_exactly_one_space,
    correct_indentation(_{state: [toplevel]}).

commas_exactly_one_space([], Out) => Out = [].
commas_exactly_one_space([white(_), comma|InRest], Out) =>
    commas_exactly_one_space([comma|InRest], Out).
commas_exactly_one_space([comma, white(_)|InRest], Out) =>
    Out = [comma, white(1)|OutRest],
    commas_exactly_one_space(InRest, OutRest).
commas_exactly_one_space([comma, Next|InRest], Out), Next \= white(_), Next \= newline =>
    Out = [comma, white(1), Next|OutRest],
    commas_exactly_one_space(InRest, OutRest).
commas_exactly_one_space([Other|Rest], Out) =>
    Out = [Other|OutRest],
    commas_exactly_one_space(Rest, OutRest).

#define(toplevel_indent, 4).

correct_indentation(State0,
                    [term_begin(Func, Arity, Parens)|InRest],
                    [term_begin(Func, Arity, Parens)|OutRest]) :-
    indent_state_top(State0, toplevel),
    Func = ':-', !,
    indent_state_push(State0, declaration, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0,
                    [term_begin(Func, Arity, Parens)|InRest],
                    [term_begin(Func, Arity, Parens)|OutRest]) :-
    indent_state_top(State0, toplevel), !,
    indent_state_push(State0, defn_head, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0,
                    [term_begin(Neckish, A, P)|InRest],
                    [term_begin(Neckish, A, P)|OutRest]) :-
    memberchk(Neckish, [':-', '=>', '-->']),
    indent_state_top(State0, defn_head), !,
    indent_state_push(State0, defn_body, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0, [newline|InRest], [newline|Out]) :-
    indent_state_top(State0, defn_body), !,
    indent_state_push(State0, defn_body_indent, State1),
    correct_indentation(State1, InRest, Out).
correct_indentation(State0, [In|InRest], Out) :-
    indent_state_top(State0, defn_body_indent), !,
    ( In = white(_)
    -> correct_indentation(State0, InRest, Out)
    ;  ( indent_state_pop(State0, State1),
         Out = [white(#toplevel_indent), In|OutRest],
         correct_indentation(State1, InRest, OutRest) )).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    functor(In, Name, _Arity, _Type),
    atom_concat(_, '_begin', Name), !,
    indent_state_push(State0, In, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    indent_state_top(State0, defn_head),
    In = term_end(_, S), S \= toplevel, !,
    % should state change to be like "expect head" or something?
    correct_indentation(State0, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    functor(In, Name, _Arity, _Type),
    atom_concat(_, '_end', Name), !,
    indent_state_pop(State0, State1),
    correct_indentation(State1, InRest, OutRest).
correct_indentation(State0, [In|InRest], [In|OutRest]) :-
    correct_indentation(State0, InRest, OutRest).
correct_indentation(_, [], []) :- !.

indent_state_top(State, Top) :-
    _{state: [Top|_]} :< State.

indent_state_push(State0, NewTop, State1) :-
    _{state: Stack} :< State0,
    put_dict(state, State0, [NewTop|Stack], State1).

indent_state_pop(State0, State1) :-
    _{state: [_|Rest]} :< State0,
    put_dict(state, State0, Rest, State1).

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
       Edit = _{
                  range: _{
                             start: _{line: LineNum, character: 0},
                             end: _{line: LineNum, character: LineLen}
                         },
                  newText: FormattedLine
              },
       Edits = [Edit|EditRest]
    ; EditRest = Edits
    ),
    succ(LineNum, LineNum1),
    create_edit_list(LineNum1, OrigRest, FormattedRest, EditRest).
