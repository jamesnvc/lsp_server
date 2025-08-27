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
:- use_module(library(dcg/high_order)).

:- include('_lsp_path_add.pl').
:- use_module(lsp(lsp_utils), [clause_variable_positions/3,
                               usemod_filespec_position/4]).

:- dynamic user:thread_message_hook/3.
:- multifile user:thread_message_hook/3.

%! check_errors(+Path:atom, -Errors:List) is det.
%
%  =Errors= is a list of the errors in the file given by =Path=.
%  This predicate changes the =user:thread_message_hook/3= hook.
check_errors(Path, Errors) :-
    nb_setval(checking_errors, []),
    Hook = (user:thread_message_hook(Term, Kind, Lines) :-
                prolog_load_context(term_position, Pos),
                stream_position_data(line_count, Pos, Line),
                stream_position_data(line_position, Pos, Char),
                nb_getval(checking_errors, ErrList),
                nb_setval(checking_errors, [e(Term, Kind, Lines, Line, Char)|ErrList])
           ),
    setup_call_cleanup(
        assertz(Hook, Ref),
        ( xref_clean(Path), xref_source(Path, [silent(false)]) ),
        erase(Ref)
    ),
    nb_getval(checking_errors, ErrList),
    once(phrase(sequence(error_expansion(Path), ErrList), Errors)).

singleton_warning_response(VarPoses, VarName) -->
    { atom_length(VarName, VarLen),
      get_assoc(VarName, VarPoses, [position(Line, Char)]),
      EndChar is Char + VarLen,
      format(string(Msg), "Singleton variable ~w", [VarName]) },
    [ @{severity: 2,
        source: "prolog_xref",
        range: @{start: @{line: Line, character: Char},
                 end:   @{line: Line, character: EndChar}},
        message: Msg
    } ].

error_expansion(Path, e(singletons(_, SingletonVars), warning, _, ClauseLine, _)) --> !,
    { clause_variable_positions(Path, ClauseLine, VariablePoses),
      list_to_assoc(VariablePoses, VarPoses) },
    sequence(singleton_warning_response(VarPoses), SingletonVars).
error_expansion(Path, e(Term, warning, _Lines, Line, _Char)) -->
    { Term = error(existence_error(file, FileSpec), _) },
    !,
    { usemod_filespec_position(Path, Line, FileSpec, Span),
      format(string(Msg), "Module `~p` not found", [FileSpec]) },
    [ @{severity: 2,
        source: "prolog_xref",
        range: Span,
        message: Msg } ].
error_expansion(_Path, e(_, silent, _, _, _)) --> !.
error_expansion(_Path, e(_Term, error, Lines, _, _)) -->
    { Lines = [url(_File:Line1:Col1), _, _, Msg0] },
    !,
    { ( Msg0 = Fmt-Params
      -> format(string(Msg), Fmt, Params)
      ;  text_to_string(Msg0, Msg) ),
      succ(Line0, Line1), ( succ(Col0, Col1) ; Col0 = 0 ) },
    [@{severity: 1,
       source: "prolog_xref",
       range: @{start: @{line: Line0, character: Col0},
                end:   @{line: Line1, character: 0}},
       message: Msg
    }].
error_expansion(Path, e(_Term, Kind, Lines, _, _)) -->
    { kind_level(Kind, Level),
      Lines = ['~w:~d:~d: '-[Path, Line1, Char1]|Msgs0], !,
      maplist(expand_error_message, Msgs0, Msgs),
      atomic_list_concat(Msgs, Msg),
      succ(Line0, Line1),
      ( succ(Char0, Char1) ; Char0 = 0 ) },
    [@{severity: Level,
       source: "prolog_xref",
       range: @{start: @{line: Line0, character: Char0},
                end:   @{line: Line1, character: 0}},
       message: Msg
    }].
% Skip unhandleable ones:
error_expansion(_Path, _Msg) --> !.

expand_error_message(Format-Args, Formatted) :-
    !, format(string(Formatted), Format, Args).
expand_error_message(Msg, Msg).

kind_level(error, 1).
kind_level(warning, 2).
