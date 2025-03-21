:- module(lsp_formatter, []).

/** <module> LSP Formatter

Module for formatting Prolog source code

@author James Cash

*/

:- use_module(lsp_formatter_parser, [ reified_format_for_file/2 ]).

file_formatted(Path, Formatted) :-
    reified_format_for_file(Path, Reified),
    apply_format_rules(Reified, Formatted).

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
