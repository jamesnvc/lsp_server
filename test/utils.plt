:- module(utils_t, []).

:- use_module(library(plunit)).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).

:- include('../prolog/_lsp_path_add.pl').

:- use_module(lsp(lsp_utils)).

:- begin_tests(utils).

ordered_locations(Ds0, Ds) :-
    maplist([D, SL-D]>>( get_dict(range, D, Range),
                         get_dict(start, Range, Start),
                         get_dict(line, Start, SL) ), Ds0, Ds1),
    sort(1, @=<, Ds1, Ds2),
    maplist([_-D, D]>>true, Ds2, Ds).

test('Basic finding',
     [ true( Locations =@= [_{range: _{start: _{line: 8, character: 4},
                                       end: _{line: 8, character: 29}}},
                            _{range: _{start: _{line: 28, character: 4},
                                       end: _{line: 28, character: 29}}},
                            _{range: _{start: _{line: 29, character: 4},
                                       end: _{line: 29, character: 29}}},
                            _{range: _{start: _{line: 35, character: 4},
                                       end: _{line: 35, character: 29}}},
                            _{range: _{start: _{line: 36, character: 4},
                                       end: _{line: 36, character: 29}}}
                           ] ) ]) :-
    module_property(formatter_t, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input1.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, file_offset_line_position/4, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

test('Finding called-by from meta-calls',
     [ true( Locations =@= [_{range: _{start: _{line: 20, character: 7},
                                       end: _{line: 20, character: 25}}},
                            _{range: _{start: _{line: 25, character: 12},
                                       end: _{line: 25, character: 30}}}] ) ]) :-
    module_property(formatter_t, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input1.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, xposition_to_match/3, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

test('finding dcg',
     [ true( Locations =@= [_{range: _{start: _{line: 18, character: 4},
                                       end: _{line: 18, character: 10}}},
                            _{range: _{start: _{line: 20, character: 4},
                                       end: _{line: 20, character: 10}}},
                            _{range: _{start: _{line: 23, character: 18},
                                       end: _{line: 23, character: 24}}}] ) ]) :-
    module_property(formatter_t, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input2.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, header/1, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

:- end_tests(utils).
