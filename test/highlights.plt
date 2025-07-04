:- module(highlights_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_highlights)).

:- use_module(library(filesex), [ relative_file_name/3 ]).
:- use_module(library(readutil), [ read_file_to_string/3 ]).

:- begin_tests(highlights).

test('Highlighting var in file location 1',
     [ true(Highlights =@= [_{range:_{end:_{character:55, line:33},
                                      start:_{character:46, line:33}}},
                            _{range:_{end:_{character:60, line:34},
                                      start:_{character:51, line:34}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    highlights_at_position(InputFile, line_char(34, 51), Highlights).


test('Highlighting var in file location 2',
     [ true(Highlights =@= [_{range:_{end:_{character:5, line:56},
                                      start:_{character:4, line:56}}},
                            _{range:_{end:_{character:11, line:63},
                                      start:_{character:10, line:63}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    highlights_at_position(InputFile, line_char(57, 4), Highlights).

test('Highlighting var in file location 3 - list tail',
     [ true(Highlights =@= [_{range:_{end:_{character:47, line:130},
                                      start:_{character:34, line:130}}},
                            _{range:_{end:_{character:81, line:132},
                                      start:_{character:68, line:132}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    highlights_at_position(InputFile, line_char(131, 45), Highlights).

test('Highlighting var in file location 4 - list element',
     [ true(Highlights =@= [_{range:_{end:_{character:89, line:127},
                                      start:_{character:81, line:127}}},
                            _{range:_{end:_{character:66, line:132},
                                      start:_{character:58, line:132}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    highlights_at_position(InputFile, line_char(133, 58), Highlights).

test('Highlighting term in file',
     [ true(Highlights =@= [_{range:_{end:_{character:13, line:24},
                                      start:_{character:4, line:24}}},
                            _{range:_{end:_{character:29, line:105},
                                      start:_{character:20, line:105}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    highlights_at_position(InputFile, line_char(25, 5), Highlights).

%%%

test('Can ask for the type of leaf to find',
     [ true(TheVar-Highlights =@=
                'LineCount'-[_{range:_{end:_{character:55, line:33},
                                       start:_{character:46, line:33}}},
                             _{range:_{end:_{character:60, line:34},
                                       start:_{character:51, line:34}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    lsp_highlights:highlights_at_position(InputFile, line_char(34, 51),
                                          '$var'(TheVar), Highlights).

test('Highlighting asking for a variable on a term should fail',
     [ fail ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    lsp_highlights:highlights_at_position(InputFile, line_char(25, 5), '$var'(_), _).

test('Highlighting asking for a variable on a comment should fail',
     [ fail ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    lsp_highlights:highlights_at_position(InputFile, line_char(130, 14), '$var'(_), _).

test('Highlighting issue with compound',
     [ true(Leaf-Highlights =@= '$var'('Bb')-[
                _{range:_{start:_{line: 143, character: 4},
                          end:_{line: 143, character: 6}}},
                _{range:_{start:_{line: 144, character: 11},
                          end:_{line: 144, character: 13}}}]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './highlight_input1.pl'),
    lsp_highlights:highlights_at_position(InputFile, line_char(145, 11), Leaf, Highlights).

:- end_tests(highlights).
