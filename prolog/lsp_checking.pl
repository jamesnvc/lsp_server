:- module(lsp_checking, [check_errors/2]).
/** <module> LSP Checking

Module for checking Prolog source files for errors and warnings.

@author James Cash
*/

:- use_module(library(apply_macros)).
:- use_module(library(assoc), [list_to_assoc/2,
                               get_assoc/3]).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [member/2]).
:- use_module(library(prolog_xref), [xref_clean/1, xref_source/1]).
:- use_module(lsp_utils, [clause_variable_positions/3]).

:- dynamic message_hook/3.
:- multifile message_hook/3.

%! check_errors(+Path:atom, -Errors:List) is det.
%
%  =Errors= is a list of the errors in the file given by =Path=.
%  This predicate changes the =user:message_hook/3= hook.
check_errors(Path, Errors) :-
    nb_setval(checking_errors, []),
    Hook = (user:message_hook(Term, Kind, Lines) :-
                prolog_load_context(term_position, Pos),
                stream_position_data(line_count, Pos, Line),
                stream_position_data(line_position, Pos, Char),
                nb_getval(checking_errors, ErrList),
                nb_setval(checking_errors, [Term-Kind-Lines-Line-Char|ErrList])
           ),
    setup_call_cleanup(
        assertz(Hook),
        ( xref_clean(Path), xref_source(Path) ),
        retractall(user:message_hook(_, _, _))
    ),
    nb_getval(checking_errors, ErrList),
    expand_errors(Path, ErrList, Errors-Errors).

expand_errors(Path, [singletons(_, SingletonVars)-warning-_-ClauseLine-_|InErrs],
              OutErrs-Tail0) :- !,
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
    expand_errors(Path, InErrs, OutErrs-Tail1).
expand_errors(Path, [_-silent-_-_-_|InErr], OutErrs-Tail) :-
    expand_errors(Path, InErr, OutErrs-Tail).
expand_errors(Path, [_Term-Kind-Lines-_-_|InErr], OutErrs-[Err|Tail]) :-
    kind_level(Kind, Level),
    Lines = ['~w:~d:~d: '-[Path, Line1, Char1]|Msgs0],
    maplist(expand_error_message, Msgs0, Msgs),
    atomic_list_concat(Msgs, Msg),
    succ(Line0, Line1),
    ( succ(Char0, Char1) ; Char0 = 0 ),
    Err = _{severity: Level,
            source: "prolog_xref",
            range: _{start: _{line: Line0, character: Char0},
                     end: _{line: Line1, character: 0}},
            message: Msg
           },
    expand_errors(Path, InErr, OutErrs-Tail).
expand_errors(Path, [_Msg|InErr], OutErrs-Tail) :-
    expand_errors(Path, InErr, OutErrs-Tail).
expand_errors(_, [], _-[]).

expand_error_message(Format-Args, Formatted) :-
    !, format(string(Formatted), Format, Args).
expand_error_message(Msg, Msg).

kind_level(error, 1).
kind_level(warning, 2).
