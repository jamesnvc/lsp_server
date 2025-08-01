:- module(formatter_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_formatter)).
:- use_module(lsp(lsp_formatter_parser)).

:- use_module(library(filesex), [relative_file_name/3]).
:- use_module(library(readutil), [ read_file_to_string/3 ]).


:- begin_tests(formatting).

test('Round-tripping file',
    [ true(OutputFileText == InputFileText),
      setup(( current_prolog_flag(encoding, PrevEncoding),
              set_prolog_flag(encoding, utf8) )),
      cleanup(set_prolog_flag(encoding, PrevEncoding))
    ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './format_input1.pl'),
    read_file_to_string(InputFile, InputFileText, [ encoding(utf8) ]),
    reified_format_for_file(InputFile, ReifiedInput),
    with_output_to(string(OutputFileText), emit_reified(current_output, ReifiedInput)).


test('Formatting example file',
    [ true(FormattedText == OutputFileText),
      setup(( current_prolog_flag(encoding, PrevEncoding),
              set_prolog_flag(encoding, utf8) )),
      cleanup(set_prolog_flag(encoding, PrevEncoding))
    ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(InputFile, ThisFile, './format_input1.pl'),
    relative_file_name(OutputFile, ThisFile, './format_output1.pl'),
    read_file_to_string(OutputFile, OutputFileText, [ encoding(utf8) ]),
    lsp_formatter:file_formatted(InputFile, Formatted),
    with_output_to(
        string(FormattedText),
        lsp_formatter_parser:emit_reified(current_output, Formatted)).

:- end_tests(formatting).
