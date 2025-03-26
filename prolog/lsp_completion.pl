:- module(lsp_completion, [completions_at/3]).
/** <module> LSP Completion

This module implements code completion, based on defined predicates in
the file & imports.

Uses =lsp_changes= in order to see the state of the buffer being edited.

@see lsp_changes:doc_text_fallback/2

@author James Cash
*/

:- use_module(library(apply), [maplist/3]).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(prolog_xref), [xref_defined/3, xref_source/2]).
:- use_module(library(yall)).

:- include('path_add.pl').

:- use_module(lsp(lsp_utils), [linechar_offset/3]).
:- use_module(lsp(lsp_changes), [doc_text_fallback/2]).

part_of_prefix(Code) :- code_type(Code, prolog_var_start).
part_of_prefix(Code) :- code_type(Code, prolog_atom_start).
part_of_prefix(Code) :- code_type(Code, prolog_identifier_continue).

get_prefix_codes(Stream, Offset, Codes) :-
    get_prefix_codes(Stream, Offset, [], Codes).

get_prefix_codes(Stream, Offset0, Codes0, Codes) :-
    peek_code(Stream, Code),
    part_of_prefix(Code), !,
    succ(Offset1, Offset0),
    seek(Stream, Offset1, bof, Offset),
    get_prefix_codes(Stream, Offset, [Code|Codes0], Codes).
get_prefix_codes(_, _, Codes, Codes).

prefix_at(File, Position, Prefix) :-
    doc_text_fallback(File, DocCodes),
    setup_call_cleanup(
        open_string(DocCodes, Stream),
        ( linechar_offset(Stream, Position, _),
          seek(Stream, -1, current, Offset),
          get_prefix_codes(Stream, Offset, PrefixCodes),
          string_codes(Prefix, PrefixCodes) ),
        close(Stream)
    ).

completions_at(File, Position, Completions) :-
    prefix_at(File, Position, Prefix),
    xref_source(File, [silent(true)]),
    findall(
        Result,
        ( xref_defined(File, Goal, _),
          functor(Goal, Name, Arity),
          atom_concat(Prefix, _, Name),
          ( predicate_arguments(File, Name, Args) -> true ; args_str(Arity, Args) ),
          format(string(Func), "~w(~w)$0", [Name, Args]),
          format(string(Label), "~w/~w", [Name, Arity]),
          Result = _{label: Label,
                     insertText: Func,
                     insertTextFormat: 2}),
        Completions,
        CompletionsTail
    ),
    findall(
        Result,
        ( predicate_property(system:Goal, built_in),
          functor(Goal, Name, Arity),
          atom_concat(Prefix, _, Name),
          \+ sub_atom(Name, 0, _, _, '$'),
          ( predicate_arguments(File, Name, Args) -> true ; args_str(Arity, Args) ),
          format(string(Func), "~w(~w)$0", [Name, Args]),
          format(string(Label), "~w/~w", [Name, Arity]),
          Result = _{label: Label,
                     insertText: Func,
                     insertTextFormat: 2}),
        CompletionsTail
    ).

predicate_arguments(File, Pred, ArgsStr) :-
    lsp_utils:predicate_help(File, Pred, HelpText),
    string_concat(Pred, "(", PredName),
    sub_string(HelpText, BeforeName, NameLen, _, PredName),
    sub_string(HelpText, BeforeClose, _, _, ")"),
    BeforeClose > BeforeName, !,
    ArgsStart is BeforeName + NameLen,
    ArgsLength is BeforeClose - ArgsStart,
    sub_string(HelpText, ArgsStart, ArgsLength, _, ArgsStr0),
    atomic_list_concat(Args, ', ', ArgsStr0),
    length(Args, Length),
    numlist(1, Length, Nums),
    maplist([Arg, Num, S]>>format(string(S), "${~w:~w}", [Num, Arg]),
           Args, Nums, Args1),
    atomic_list_concat(Args1, ', ', ArgsStr).

args_str(Arity, Str) :-
    numlist(1, Arity, Args),
    maplist([A, S]>>format(string(S), "${~w:_}", [A]),
           Args, ArgStrs),
    atomic_list_concat(ArgStrs, ', ', Str).
