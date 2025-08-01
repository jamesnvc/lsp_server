:- module(checking_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_checking)).

:- use_module(library(filesex), [relative_file_name/3]).
:- use_module(library(readutil), [ read_file_to_string/3 ]).

:- begin_tests(checking).

test('Errors for test file',
     [ true( Errors =@=  [_{ message: "Operator expected",
                              range: _{ end: _{ character: 0, line: 10 },
                                      start: _{ character: 11, line: 9 } },
                              severity: 1,
                              source: "prolog_xref" },
                          _{ message: "Singleton variable Gae",
                              range: _{ end: _{ character: 7, line: 4 },
                                      start: _{ character: 4, line: 4 } },
                              severity: 2,
                              source: "prolog_xref" },
                          _{ message: "Singleton variable B",
                              range: _{ end: _{ character: 10, line: 6 },
                                      start: _{ character: 9, line: 6 } },
                              severity: 2,
                              source: "prolog_xref" },
                          _{ message: "Singleton variable Z",
                              range: _{ end: _{ character: 12, line: 7 },
                                      start: _{ character: 11, line: 7 } },
                              severity: 2,
                              source: "prolog_xref" },
                          _{ message: "Module `nexiste_pas` not found",
                              range: _{ end: _{ character: 25, line: 1 },
                                      start: _{ character: 14, line: 1 } },
                              severity: 2,
                              source: "prolog_xref" },
                          _{ message: "Module `library(does_not_exist)` not found",
                              range: _{ end: _{ character: 37, line: 0 },
                                      start: _{ character: 14, line: 0 } },
                              severity: 2,
                              source: "prolog_xref" } ]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './checking_input1.pl'),
    check_errors(InputFile, Errors).

:- end_tests(checking).
