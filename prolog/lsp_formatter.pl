:- module(lsp_formatter, [ file_format_edits/2 ]).

/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(library(readutil), [ read_file_to_string/3 ]).

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
    comments_exactly_one_space.

comments_exactly_one_space([], Out) => Out = [].
comments_exactly_one_space([white(_), comma|InRest], Out) =>
    comments_exactly_one_space([comma|InRest], Out).
comments_exactly_one_space([comma, white(_)|InRest], Out) =>
    Out = [comma, white(1)|OutRest],
    comments_exactly_one_space(InRest, OutRest).
comments_exactly_one_space([comma, Next|InRest], Out), Next \= white(_), Next \= newline =>
    Out = [comma, white(1), Next|OutRest],
    comments_exactly_one_space(InRest, OutRest).
comments_exactly_one_space([Other|Rest], Out) =>
    Out = [Other|OutRest],
    comments_exactly_one_space(Rest, OutRest).

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
