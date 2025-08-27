:- module(checking_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_checking)).

:- use_module(library(filesex), [relative_file_name/3]).
:- use_module(library(readutil), [ read_file_to_string/3 ]).

:- begin_tests(checking).

test('Errors for test file',
     [ true( Errors =@=  [@{ message: "Operator expected",
                              range: @{ end: @{ character: 0, line: 10 },
                                      start: @{ character: 11, line: 9 } },
                              severity: 1,
                              source: "prolog_xref" },
                          @{ message: "Singleton variable Gae",
                              range: @{ end: @{ character: 7, line: 4 },
                                      start: @{ character: 4, line: 4 } },
                              severity: 2,
                              source: "prolog_xref" },
                          @{ message: "Singleton variable B",
                              range: @{ end: @{ character: 10, line: 6 },
                                      start: @{ character: 9, line: 6 } },
                              severity: 2,
                              source: "prolog_xref" },
                          @{ message: "Singleton variable Z",
                              range: @{ end: @{ character: 12, line: 7 },
                                      start: @{ character: 11, line: 7 } },
                              severity: 2,
                              source: "prolog_xref" },
                          @{ message: "Module `nexiste_pas` not found",
                              range: @{ end: @{ character: 25, line: 1 },
                                      start: @{ character: 14, line: 1 } },
                              severity: 2,
                              source: "prolog_xref" },
                          @{ message: "Module `library(does_not_exist)` not found",
                              range: @{ end: @{ character: 37, line: 0 },
                                      start: @{ character: 14, line: 0 } },
                              severity: 2,
                              source: "prolog_xref" } ]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './checking_input1.pl'),
    check_errors(InputFile, Errors).

:- end_tests(checking).
