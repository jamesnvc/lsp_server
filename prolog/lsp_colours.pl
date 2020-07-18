:- module(lsp_colours, [file_colours/2,
                        file_range_colours/4,
                        token_types/1,
                        token_modifiers/1]).

:- use_module(library(apply), [maplist/4]).
:- use_module(library(apply_macros)).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [numlist/3, member/2, nth0/3]).
:- use_module(library(prolog_colour), [prolog_colourise_stream/3,
                                       prolog_colourise_term/4]).
:- use_module(library(prolog_source), [read_source_term_at_location/3]).
:- use_module(library(yall)).

:- use_module(lsp_changes, [doc_text/2]).
:- use_module(lsp_utils, [seek_to_line/2,
                          linechar_offset/3]).

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

%! file_colours(+File, -Colours) is det.
%
%  True when =Colours= is a list of colour information
%  corresponding to the file =File=.
file_colours(File, Tuples) :-
    setup_call_cleanup(
        message_queue_create(Queue),
        ( thread_create(file_colours_helper(Queue, File), ThreadId),
          await_messages(Queue, Colours0, Colours0) ),
        ( thread_join(ThreadId),
          message_queue_destroy(Queue) )
    ),
    sort(2, @=<, Colours0, Colours),
    flatten_colour_terms(File, Colours, Tuples).

file_range_colours(File, Start, End, Tuples) :-
    setup_call_cleanup(
        message_queue_create(Queue),
        ( thread_create(file_term_colours_helper(Queue, File, Start, End),
                        ThreadId),
          await_messages(Queue, Colours0, Colours0) ),
        ( thread_join(ThreadId),
          message_queue_destroy(Queue) )
    ),
    sort(2, @=<, Colours0, Colours),
    flatten_colour_terms(File, Colours, Tuples).

file_stream(File, S) :-
    doc_text(File, Changes)
    -> open_string(Changes, S)
    ;  open(File, read, S).

%! flatten_colour_terms(+File, +ColourTerms, -Nums) is det.
%
%  Convert the list of =ColourTerms= like =colour(Category, Start,
%  Length)= to a flat list of numbers in the format that LSP expects.
%
%  @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#textDocument_semanticTokens
flatten_colour_terms(File, ColourTerms, Nums) :-
    token_types_dict(TokenDict),
    setup_call_cleanup(
        file_stream(File, S),
        ( set_stream_position(S, '$stream_position'(0,0,0,0)),
          colour_terms_to_tuples(ColourTerms, Nums-Nums,
                                 S, TokenDict,
                                 0, 0, 0) ),
        close(S)
    ).

colour_terms_to_tuples([], _-[],
                       _Stream, _Dict,
                       _Offset, _Line, _Char).
colour_terms_to_tuples([Colour|Colours], Tuples-T0,
                       Stream, Dict,
                       LastOffset, LastLine, LastChar) :-
    colour_term_to_tuple(Stream, Dict,
                         LastOffset, LastLine, LastChar,
                         ThisOffset, ThisLine, ThisChar,
                         Colour,
                         T0-T1), !,
    colour_terms_to_tuples(Colours, Tuples-T1,
                           Stream, Dict,
                           ThisOffset, ThisLine, ThisChar).
colour_terms_to_tuples([colour(_Type, _, _)|Colours], Tuples,
                       Stream, Dict,
                       ThisOffset, ThisLine, ThisChar) :-
    %% ( Type = goal_term(T, _)
    %% -> debug(server, "Unhighlighted goal term ~w", [T])
    %% ; true),
    colour_terms_to_tuples(Colours, Tuples,
                           Stream, Dict,
                           ThisOffset, ThisLine, ThisChar).

colour_term_to_tuple(Stream, Dict,
                     LastOffset, LastLine, LastChar,
                     Offset, Line, Char,
                     colour(Type, Offset, Len),
                     [DeltaLine, DeltaStart, Len, TypeCode, ModMask|T1]-T1) :-
    colour_type(Type, TypeCategory, Mods),
    get_dict(TypeCategory, Dict, TypeCode),
    mods_mask(Mods, ModMask), !,
    Seek is Offset - LastOffset,
    setup_call_cleanup(open_null_stream(NullStream),
                       copy_stream_data(Stream, NullStream, Seek),
                       close(NullStream)),
    stream_property(Stream, position(Pos)),
    stream_position_data(line_count, Pos, Line),
    stream_position_data(line_position, Pos, Char),
    ( Line == LastLine
    -> ( DeltaLine = 0,
         DeltaStart is Char - LastChar
       )
    ; ( DeltaLine is Line - LastLine,
        DeltaStart = Char
      )
    ).

% colour_type(directive,              macro,     []).
colour_type(neck(directive),          operator,  []).
colour_type(neck(clause),             operator,  []).
colour_type(goal_term(built_in,       A),        macro,     []) :-
    atom(A).
colour_type(goal_term(undefined,      _),        function,  []).
colour_type(goal_term(imported(_),    _),        function,  []).
colour_type(goal_term(('dynamic'(_)), _),      parameter, []).
colour_type(atom,                     enum,      []).
colour_type(var,                      variable,  []).
colour_type(fullstop,                 operator,  []).
colour_type(control,                  operator,  []).
colour_type(dict_key,                 property,  []).
colour_type(dict_sep,                 operator,  []).
colour_type(string,                   string,    []).
colour_type(int,                      number,    []).
colour_type(comment(line),            comment,   []).
colour_type(comment(structured),      comment,   [documentation]).
colour_type(arity,                    parameter, []).
colour_type(functor,                  struct,    []).

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
        file_stream(File, S),
        prolog_colourise_stream(
            S, File,
            {Queue}/[Cat, Start, Len]>>(
                thread_send_message(Queue, colour(Cat, Start, Len)))
        ),
        close(S)
    ),
    thread_send_message(Queue, done).

nearest_term_start(Stream, StartL, TermStart) :-
    read_source_term_at_location(Stream, _, [line(StartL), error(Error)]),
    ( nonvar(Error)
    -> ( LineBack is StartL - 1,
         nearest_term_start(Stream, LineBack, TermStart) )
    ;  TermStart = StartL
    ).

file_term_colours_helper(Queue, File,
                         line_char(StartL, _StartC),
                         End) :-
    setup_call_cleanup(
        file_stream(File, S),
        ( nearest_term_start(S, StartL, TermLine),
          seek(S, 0, bof, _),
          set_stream_position(S, '$stream_position'(0,0,0,0)),
          seek_to_line(S, TermLine),
          colourise_terms_to_position(Queue, File, S, 0-0, End)
        ),
        close(S)
    ),
    thread_send_message(Queue, done).

colourise_terms_to_position(Queue, File, Stream, Prev, End) :-
    prolog_colourise_term(
        Stream, File,
        {Queue}/[Cat, Start, Len]>>(
            thread_send_message(Queue, colour(Cat, Start, Len))),
        []),
    stream_property(Stream, position(Pos)),
    stream_position_data(line_count, Pos, Line),
    stream_position_data(line_position, Pos, Char),
    End = line_char(EndL, EndC),
    ( Line-Char == Prev
    -> true
    ;  EndL =< Line
    -> true
    ;  ( EndL == Line, EndC =< Char )
    -> true
    ; colourise_terms_to_position(Queue, File, Stream, Line-Char, End)
    ).
