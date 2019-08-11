:- module(lsp_checking, [check_errors/2]).

:- use_module(library(dcg/basics), [string//1,
                                    string_without//2,
                                    whites//0,
                                    integer//1]).

check_errors(Path, Errors) :-
    with_output_to_codes(
        setup_call_cleanup(
            ( stream_property(OldErr, alias(user_error)),
              set_stream(OutStream, alias(user_error))
            ),
            xref_source(Path),
            set_stream(OldErr, alias(user_error))
        ),
        OutStream,
        ErrorCodes,
        []),
    ( phrase(error_messages(Errors), ErrorCodes)
    -> true
    ; debug(server, "Couldn't parse errors '~s'", [ErrorCodes]) ).

error_message(_{severity: 1,
                source: "prolog_xref",
                file: Path,
                range: _{start: _{line: Line0, character: Char0},
                        end: _{line: Line1, character: 0}},
                message: Msg}) -->
    "ERROR: ", string(PathCodes), ":", integer(Line1), ":", integer(Char1), ":",
    " ", string_without("\n", MsgCodes), "\n",
    { succ(Line0, Line1), succ(Char0, Char1),
      string_codes(Msg, MsgCodes),
      atom_codes(Path, PathCodes) }.
error_message(_{severity: 2,
                source: "prolog_xref",
                file: Path,
                range: _{start: _{line: Line0, character: 0},
                         % [TODO] make this end at the end of the clause
                         end: _{line: Line1, character: 0}},
                message: Msg}) -->
    "Warning: ", string(PathCodes), ":", integer(Line1), ":\n",
    "Warning: ", whites, string_without("\n", MsgCodes), "\n",
    { succ(Line0, Line1),
      atom_codes(Path, PathCodes),
      string_codes(Msg, MsgCodes) }.

error_messages([Error|Errors]) -->
    error_message(Error), !,
    error_messages(Errors).
error_messages([]) --> [].
