:- module(changes_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').

:- use_module(lsp(lsp_changes)).

:- begin_tests(changes).

test('Replacing range 1', [ true( Changed =@= `foo\nbart\nbaz\n` )]) :-
    lsp_changes:replace_codes_range(`foo\nbar\nbaz\n`, 1, 3, 1, 3, `t`, Changed).

test('Replacing range 2', [ true( Changed =@= `foo\nbat\nbaz\n` )]) :-
    lsp_changes:replace_codes_range(`foo\nbar\nbaz\n`, 1, 2, 1, 3, `t`, Changed).

test('Replacing range 3', [ true( Changed =@= `for\nrrr\nraz\n` )]) :-
    lsp_changes:replace_codes_range(`foo\nbar\nbaz\n`, 0, 2, 2, 1, `r\nrrr\nr`, Changed).

:- end_tests(changes).
