:- module(lsp_checking, [check_errors/2]).

:- use_module(library(dcg/basics), [string//1,
                                    string_without//2,
                                    prolog_var_name//1,
                                    whites//0,
                                    integer//1]).
:- use_module(lsp_utils, [clause_variable_positions/3]).

check_errors(Path, ExErrors) :-
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
    phrase(error_messages(AllErrors), ErrorCodes),
    convlist({Path}/[Err, FErr]>>(
                 del_dict(file, Err, Path, FErr)
             ),
            AllErrors,
            Errors),
    maybe_expand_errors(Path, Errors, ExErrors-ExErrors).

maybe_expand_errors(Path, [Err|InErrs], OutErrs-Tail0) :-
    string_concat("Singleton variables: ", SingleVarsS, Err.message), !,
    string_codes(SingleVarsS, SingleVarsCodes),
    phrase(parse_list(SingletonVars), SingleVarsCodes),
    succ(Err.range.start.line, ClauseLine),
    clause_variable_positions(Path, ClauseLine, VariablePoses),
    list_to_assoc(VariablePoses, VarPoses),
    findall(
        NewErr,
        ( member(VarName, SingletonVars),
          atom_length(VarName, VarLen),
          get_assoc(VarName, VarPoses, [position(Line, Char)]),
          EndChar is Char + VarLen,
          format(string(Msg), "Singleton variable ~w", [VarName]),
          NewErr = _{severity: 2,
                  source: "prolog_xref",
                  range: _{start: _{line: Line, character: Char},
                           end: _{line: Line, character: EndChar}},
                  message: Msg}),
        Tail0,
        Tail1),
    maybe_expand_errors(Path, InErrs, OutErrs-Tail1).
maybe_expand_errors(Path, [Err|InErrs], OutErrs-[Err|Tail]) :-
    !,
    maybe_expand_errors(Path, InErrs, OutErrs-Tail).
maybe_expand_errors(_, [], _-[]).

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

parse_list(Xs) --> "[", parse_elts(Xs).
parse_elts([X|Xs]) -->
    prolog_var_name(X), !, parse_next_elts(Xs).
parse_next_elts([]) --> "]".
parse_next_elts(Xs) --> ",", parse_elts(Xs).
