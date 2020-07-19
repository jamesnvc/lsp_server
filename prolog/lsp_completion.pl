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
:- use_module(lsp_utils, [linechar_offset/3]).
:- use_module(lsp_changes, [doc_text_fallback/2]).

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
          args_str(Arity, Args),
          format(string(Func), "~w(~w)$0", [Name, Args]),
          format(string(Label), "~w/~w", [Name, Arity]),
          Result = _{label: Label,
                     insertText: Func,
                     insertTextFormat: 2}),
        Completions
    ).

args_str(Arity, Str) :-
    numlist(1, Arity, Args),
    maplist([A, S]>>format(string(S), "${~w:_}", [A]),
           Args, ArgStrs),
    atomic_list_concat(ArgStrs, ', ', Str).
