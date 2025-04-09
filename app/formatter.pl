:- use_module(library(main)).

:- include('../prolog/_lsp_path_add.pl').

:- use_module(lsp(lsp_formatter), [ file_formatted/2 ]).
:- use_module(lsp(lsp_formatter_parser), [ emit_reified/2 ]).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Files, Options),
    check_options(Options),
    main(Files, Options).

check_options(Opts) :-
    memberchk(in_place(true), Opts),
    memberchk(output_to(_), Opts), !,
    format(user_error, "If inplace is true, output-to doesn't make sense~n", []),
    halt(1).
check_options(_).


opt_type(inplace, in_place, boolean(true)).
opt_type(output_to, output_to, file).

opt_help(help(header),
         ["Reformat Prolog files.~n",
          "If ", ansi(bold, "~w", ["inplace"]),
          " is false and more than one file is specified, ",
          ansi(bold, "~w", ["output-to"]), " is interpretted as a directory to output"]).
opt_help(help(usage), " [option ...] file ...").
opt_help(inplace, "Re-format file in place").
opt_help(output_to, "If IN_PLACE is false, file to output reformatted file to").

main([File], Options) :-
    \+ memberchk(in_place(false), Options), !,
    file_formatted(File, Formatted),
    setup_call_cleanup(
        open(File, write, S),
        emit_reified(S, Formatted),
        close(S)
    ).
main([File], Options) :-
    memberchk(in_place(false), Options), !,
    ( memberchk(output_to(Output), Options)
    -> true ; Output = '-' ),
    file_formatted(File, Formatted),
    ( Output = '-'
    -> emit_reified(user_output, Formatted)
    ; setup_call_cleanup(
          open(Output, write, S),
          emit_reified(S, Formatted),
          close(S)
      )
    ).
