:- module(utils_t, []).

:- use_module(library(plunit)).

:- use_module(library(apply), [maplist/3]).
:- use_module(library(apply_macros)).
:- use_module(library(yall)).

:- include('../prolog/_lsp_path_add.pl').

:- use_module(lsp(lsp_utils)).

:- begin_tests(utils).

:- encoding(utf8).

ordered_locations(Ds0, Ds) :-
    maplist([D, SL-D]>>( get_dict(range, D, Range),
                         get_dict(start, Range, Start),
                         get_dict(line, Start, SL) ), Ds0, Ds1),
    sort(1, @=<, Ds1, Ds2),
    maplist([_-D, D]>>true, Ds2, Ds).

test('Basic finding',
     [ true( Locations =@= [@{range: @{start: @{line: 8, character: 4},
                                       end: @{line: 8, character: 29}}},
                            @{range: @{start: @{line: 28, character: 4},
                                       end: @{line: 28, character: 29}}},
                            @{range: @{start: @{line: 29, character: 4},
                                       end: @{line: 29, character: 29}}},
                            @{range: @{start: @{line: 35, character: 4},
                                       end: @{line: 35, character: 29}}},
                            @{range: @{start: @{line: 36, character: 4},
                                       end: @{line: 36, character: 29}}}
                           ] ) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input1.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, file_offset_line_position/4, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

test('Finding called-by from meta-calls',
     [ true( Locations =@= [@{range: @{start: @{line: 20, character: 7},
                                       end: @{line: 20, character: 25}}},
                            @{range: @{start: @{line: 25, character: 12},
                                       end: @{line: 25, character: 30}}}] ) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input1.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, xposition_to_match/3, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

test('finding dcg',
     [ true( Locations =@= [@{range: @{start: @{line: 18, character: 4},
                                       end: @{line: 18, character: 10}}},
                            @{range: @{start: @{line: 20, character: 4},
                                       end: @{line: 20, character: 10}}},
                            @{range: @{start: @{line: 23, character: 18},
                                       end: @{line: 23, character: 24}}}] ) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input2.pl'),
    setup_call_cleanup(
        xref_source(InputFile),
        called_at(InputFile, header/1, Locations0),
        xref_clean(InputFile)
    ),
    ordered_locations(Locations0, Locations).

test('help at position',
    [ true( Help =@= 'succ(?Int1, ?Int2)
Availability: built-in


True if Int2 = Int1 + 1 and Int1 ≥.  At least  one of  the arguments
must be instantiated to a natural number. This predicate  raises the
domain error not_less_than_zero if called  with a  negative integer.
E.g.  succ(X,  0)  fails silently  and succ(X,  -1) raises  a domain
error.') ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './utils_input1.pl'),
    help_at_position(InputFile, 38, 5, Help).

:- end_tests(utils).
