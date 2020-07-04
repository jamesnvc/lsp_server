:- module(lsp_colours, [file_colours/2,
                        token_types/1,
                        token_modifiers/1]).

:- use_module(library(apply), [maplist/4]).
:- use_module(library(apply_macros)).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [numlist/3, member/2, nth0/3]).
:- use_module(library(prolog_colour), [prolog_colourise_stream/3]).
:- use_module(library(yall)).

:- use_module(lsp_utils, [offset_line_char/3]).

token_types([namespace,
             type,
             parameter,
             variable,
             function,
             comment,
             string,
             number,
             operator,
             macro,
             enum,
             class,
             property,
             struct
            ]).
token_modifiers([declaration,
                 definition,
                 documentation]).

token_types_dict(Dict) :-
    token_types(Types),
    length(Types, Len),
    Len0 is Len - 1,
    numlist(0, Len0, Ns),
    maplist([Type, Idx, Type-Idx]>>true, Types, Ns,
            Pairs),
    dict_create(Dict, _, Pairs).


%! file_colors(+File, -Colours) is det.
%
%  True when =Colours= is a list of colour information
%  corresponding to the file =File=.
file_colours(File, Tuples) :-
    setup_call_cleanup(
        message_queue_create(Queue),
        ( thread_create(file_colours_helper(Queue, File), ThreadId),
          await_messages(Queue, Colours, Colours) ),
        ( thread_join(ThreadId),
          message_queue_destroy(Queue) )
    ),
    flatten_colour_terms(File, Colours, Tuples).

%! flatten_colour_terms(+File, +ColourTerms, -Nums) is det.
%
%  Convert the list of =ColourTerms= like =colour(Category, Start,
%  Length)= to a flat list of numbers in the format that LSP expects.
%
%  @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#textDocument_semanticTokens
flatten_colour_terms(File, ColourTerms, Nums) :-
    token_types_dict(TokenDict),
    setup_call_cleanup(
        open(File, read, S),
        colour_terms_to_tuples(ColourTerms, Nums-Nums,
                               S, TokenDict,
                               0, 0),
        close(S)
    ).

colour_terms_to_tuples([], _-[], _, _, _, _).
colour_terms_to_tuples([Colour|Colours], Tuples-T0,
                       Stream, Dict,
                       LastLine, LastChar) :-
    colour_term_to_tuple(Stream, Dict,
                         LastLine, LastChar,
                         ThisLine, ThisChar,
                         Colour,
                         T0-T1), !,
    colour_terms_to_tuples(Colours, Tuples-T1,
                           Stream, Dict,
                           ThisLine, ThisChar).
colour_terms_to_tuples([colour(Type, _, _)|Colours], Tuples, Stream, Dict,
                       ThisLine, ThisChar) :-
    debug(server, "Unhighlighted ~w", [Type]),
    colour_terms_to_tuples(Colours, Tuples,
                           Stream, Dict,
                           ThisLine, ThisChar).

colour_term_to_tuple(Stream, Dict, LastLine, LastChar,
                     Line, Char,
                     colour(Type, Offset, Len),
                     [DeltaLine, DeltaStart, Len, TypeCode, ModMask|T1]-T1) :-
    colour_type(Type, TypeCategory, Mods),
    get_dict(TypeCategory, Dict, TypeCode), !,
    mods_mask(Mods, ModMask),
    offset_line_char(Stream, Offset, position(Line, Char)),
    ( Line == LastLine
    -> ( DeltaLine = 0,
         DeltaStart is Char - LastChar
       )
    ; ( DeltaLine is Line - LastLine,
        DeltaStart = Char
      )
    ).

% colour_type(directive,           macro,     []).
colour_type(neck(directive),       operator,  []).
colour_type(neck(clause),          operator,  []).
colour_type(goal_term(built_in,    A),        macro,    []) :-
    atom(A).
colour_type(goal_term(undefined,   _),        function, []).
colour_type(goal_term(imported(_), _),        function, []).
colour_type(atom,                  enum,      []).
colour_type(var,                   variable,  []).
colour_type(fullstop,              operator,  []).
colour_type(control,               operator,  []).
colour_type(dict_key,              property,  []).
colour_type(dict_sep,              operator,  []).
colour_type(string,                string,    []).
colour_type(int,                   number,    []).
colour_type(comment(line),         comment,   []).
colour_type(comment(structured),   comment,   [documentation]).
colour_type(arity,                 parameter, []).
colour_type(functor,                struct, []).

mods_mask(Mods, Mask) :-
    mods_mask(Mods, 0, Mask).

mods_mask([], Mask, Mask).
mods_mask([Mod|Mods], Mask0, Mask) :-
    token_modifiers(ModsList),
    nth0(N, ModsList, Mod),
    Mask1 is Mask0 \/ (1 << N),
    mods_mask(Mods, Mask1, Mask).

%%% Helpers

%! await_messages(+Queue, ?Head, -Tail) is det.
%
%  Helper predicate to accumulate messages from
%  =file_colours_helper/2= in a list.
await_messages(Q, H, T) :-
    thread_get_message(Q, Term),
    ( Term == done
    -> T = []
    ; ( T = [Term|T0],
        await_messages(Q, H, T0)
      )
    ).

%! file_colours_helper(+Queue, +File) is det.
%
%  Use =prolog_colourise_stream/3= to accumulate a list of colour
%  terms. Does it in this weird way sending messages to a queue
%  because the predicate takes a closure but we want to get a list of
%  all of the terms.
file_colours_helper(Queue, File) :-
    setup_call_cleanup(
        open(File, read, S),
        prolog_colourise_stream(
            S, File,
            {Queue}/[Cat, Start, Len]>>(
                thread_send_message(Queue, colour(Cat, Start, Len)))
        ),
        close(S)
    ),
    thread_send_message(Queue, done).
