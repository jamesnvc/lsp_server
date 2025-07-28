:- module(colours_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_colours)).

:- begin_tests(colours).

test('Basic highlighting',
    [ condition(\+ current_prolog_flag(windows, true) ),
      true(Colours = [0,0,2,21,1,0,0,19,0,0,0,3,16,12,512,0,7,4,0,0,0,9,1,21,0,2,
                      0,2,21,1,0,0,101,0,0,0,3,98,12,512,0,11,14,0,0,1,4,9,14,16,
                      0,11,14,17,0,1,1,9,14,16,0,11,14,17,0,1,4,7,14,16,1,2,1,21,
                      0,2,0,6,12,1,0,4,1,8,0,0,3,2,21,1,1,9,7,12,512,0,11,8,18,0,
                      0,11,1,19,0,0,3,1,8,0,0,3,1,21,0,0,2,10,17,0,1,4,17,17,0,1,
                      12,4,12,512,0,7,1,19,0,0,2,1,19,0,0,2,1,19,0,0,4,1,8,0,0,2,
                      1,21,0]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './colours_input1.pl'),
    file_colours(InputFile, Colours).

% windows newlines change the length of multi-line highlight regions
test('Basic highlighting (Windows)',
    [ condition(current_prolog_flag(windows, true) ),
      true(Colours = [0,0,2,21,1,0,0,19,0,0,0,3,16,12,512,0,7,4,0,0,0,9,1,21,0,2,
                      0,2,21,1,0,0,105,0,0,0,3,102,12,512,0,11,14,0,0,1,4,9,14,
                      16,0,11,15,17,0,1,1,9,14,16,0,11,15,17,0,1,4,7,14,16,1,2,1,
                      21,0,2,0,6,12,1,0,4,1,8,0,0,3,2,21,1,1,9,7,12,512,0,11,8,
                      18,0,0,11,1,19,0,0,3,1,8,0,0,3,1,21,0,0,2,11,17,0,1,4,18,
                      17,0,1,12,4,12,512,0,7,1,19,0,0,2,1,19,0,0,2,1,19,0,0,4,1,
                      8,0,0,2,1,21,0]) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './colours_input1_win.pl'),
    file_colours(InputFile, Colours).


:- end_tests(colours).
