:- module(lsp_completion, [completions_at/3]).

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
        ( linechar_offset(Stream, Position, Offset),
          get_prefix_codes(Stream, Offset, PrefixCodes),
          string_codes(Prefix, PrefixCodes) ),
        close(Stream)
    ).

completions_at(File, Position, Completions) :-
    prefix_at(File, Position, Prefix),
    debug(completion, "completions for ~w", [Prefix]),
    xref_source(File, [silent(true)]),
    findall(
        Result,
        ( xref_defined(File, Goal, _),
          functor(Goal, Name, _),
          debug(completion, "name ~w", [Name]),
          atom_concat(Prefix, _, Name),
          Result = _{label: Name} ),
        Completions
    ).
