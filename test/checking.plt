:- module(checking_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_checking)).

:- use_module(library(filesex), [relative_file_name/3]).
:- use_module(library(readutil), [ read_file_to_string/3 ]).

:- begin_tests(checking).

test('Errors for test file',
     [ true( Errors =@= [_{message: "Operator expected",
                           range: _{start: _{line: 6, character: 11}, end: _{line: 7, character: 0}},
                           severity: 1,
                           source: "prolog_xref"},
                         _{message: "Singleton variable Gae",
                           range: _{start: _{line: 1, character: 4}, end: _{line: 1, character: 7}},
                           severity: 2,
                           source: "prolog_xref"},
                         _{message: "Singleton variable B",
                           range: _{start: _{line: 3, character: 9}, end: _{line: 3, character: 10}},
                           severity: 2,
                           source: "prolog_xref"},
                         _{message: "Singleton variable Z",
                           range: _{start: _{line: 4, character: 11}, end: _{line: 4, character: 12}},
                           severity: 2,
                           source: "prolog_xref"}] ) ]) :-
    module_property(formatter_t, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './checking_input1.pl'),
    check_errors(InputFile, Errors).

:- end_tests(checking).
