:- module(lsp_colours, [file_colours/2,
                        file_range_colours/4,
                        token_types/1,
                        token_modifiers/1]).
/** <module> LSP Colours

Module with predicates for colourizing Prolog code, via library(prolog_colour).

@author James Cash
*/

:- use_module(library(apply), [maplist/4]).
:- use_module(library(apply_macros)).
:- use_module(library(debug), [debug/3]).
:- use_module(library(lists), [numlist/3, nth0/3]).
:- use_module(library(prolog_colour), [prolog_colourise_stream/3,
                                       prolog_colourise_term/4]).
:- use_module(library(prolog_source), [read_source_term_at_location/3]).
:- use_module(library(yall)).

:- include('_lsp_path_add.pl').
:- use_module(lsp(lsp_changes), [doc_text/2]).
:- use_module(lsp(lsp_utils), [seek_to_line/2,
                               linechar_offset/3]).

token_types([namespace,
             type,
             class,
             enum,
             interface,
             struct,
             typeParameter,
             parameter,
             variable,
             property,
             enumMember,
             event,
             function,
             member,
             macro,
             keyword,
             modifier,
             comment,
             string,
             number,
             regexp,
             operator
            ]).
token_modifiers([declaration,
                 definition,
                 readonly,
                 static,
                 deprecated,
                 abstract,
                 async,
                 modification,
                 documentation,
                 defaultLibrary
                ]).

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
    file_colours_helper(File, Colours0),
    sort(2, @=<, Colours0, Colours),
    flatten_colour_terms(File, Colours, Tuples).

%! file_range_colours(+File, +Start, +End, -Colours) is det.
%
%  True when =Colours= is a list of colour information corresponding
%  to file =File= covering the terms between =Start= and =End=. Note
%  that it may go beyond either bound.
file_range_colours(File, Start, End, Tuples) :-
    file_term_colours_helper(File, Start, End, Colours0),
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
        ( set_stream(S, newline(posix)),
          set_stream_position(S, '$stream_position'(0,1,0,0)),
          colour_terms_to_tuples(ColourTerms, Nums-Nums,
                                 S, TokenDict,
                                 0, 1, 0) ),
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
    % ( memberchk(Type, [clause, body, list, empty_list, brace_term, parentheses,
    %                    range, goal(_, _), head(_, _), dict, dict_content,
    %                    term, error])
    % -> true
    % ; debug(server, "Unhighlighted term ~w", [Type])
    % ),
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
    % can't use line_position, because it counts tabs as 8 characters, which throws things off
    % stream_position_data(line_position, Pos, Char),
    line_position_characters(Stream, Pos, Char),
    ( Line == LastLine
    -> ( DeltaLine = 0,
         DeltaStart is Char - LastChar
       )
    ; ( DeltaLine is Line - LastLine,
        DeltaStart = Char
      )
    ).

line_position_characters(Stream, Pos, Char) :-
    stream_position_data(char_count, Pos, StartChar),
    stream_position_data(line_count, Pos, StartLine),
    ( StartLine > 1
    -> ( repeat,
         seek(Stream, -1, current, _),
         peek_code(Stream, 0'\n), !,
         get_code(Stream, _) )
    ; seek(Stream, 0, bof, _) ),
    character_count(Stream, StartOfLine),
    ( repeat,
      character_count(Stream, CharHere),
      get_code(Stream, _),
      CharHere >= StartChar, !
    ),
    Char is max(0, CharHere - StartOfLine),
    set_stream_position(Stream, Pos).

colour_type(directive,                namespace, []).

colour_type(head_term(_,              _),        function,  [declaration]).
colour_type(neck(directive),          operator,  [declaration]).
colour_type(neck(':-'),               operator,  [declaration]).
colour_type(neck(clause),             operator,  [definition]).
colour_type(neck(grammar_rule),       operator,  [definition]).
colour_type(goal_term(built_in,       A),        macro,     []) :- atom(A), !.
colour_type(goal_term(built_in,       _),        function,  [defaultLibrary]).
colour_type(goal_term(undefined,      _),        function,  []).
colour_type(goal_term(imported(_),    _),        function,  []).
colour_type(goal_term(local(_),       _),        function,  []).
colour_type(goal_term(extern(_,_),    _),        function,  []).
colour_type(goal_term(recursion,      _),        member,    []).
colour_type(goal_term(('dynamic'(_)), _),        member, []).
colour_type(atom,                     string,    []).
colour_type(var,                      variable,  []).
colour_type(singleton,                variable,  [readonly]).
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
colour_type(option_name,              struct,    []).
colour_type(predicate_indicator,      interface, []).
colour_type(predicate_indicator(_,    _),        interface, []).
colour_type(unused_import,            macro,     [deprecated]).
colour_type(undefined_import,         macro,     [deprecated]).
colour_type(dcg,                      regexp,    []).
colour_type(dcg(terminal),            regexp,    []).
colour_type(dcg(plain),               function,  []).
colour_type(dcg_right_hand_ctx,       regexp,    []).
colour_type(grammar_rule,             regexp,    []).
colour_type(identifier,               namespace, []).
colour_type(file(_),                  namespace, []).
colour_type(file_no_depend(_),        namespace, [abstract]).
colour_type(module(_),                namespace, []).

mods_mask(Mods, Mask) :-
    mods_mask(Mods, 0, Mask).

mods_mask([], Mask, Mask).
mods_mask([Mod|Mods], Mask0, Mask) :-
    token_modifiers(ModsList),
    nth0(N, ModsList, Mod),
    Mask1 is Mask0 \/ (1 << N),
    mods_mask(Mods, Mask1, Mask).

%%% Helpers

%! file_colours_helper(+Queue, +File) is det.
%
%  Use =prolog_colourise_stream/3= to accumulate a list of colour
%  terms.
file_colours_helper(File, Info) :-
    Acc = acc([]),
    setup_call_cleanup(
        file_stream(File, S),
        prolog_colourise_stream(
            S, File,
            {Acc}/[Cat, Start, Len]>>(
                arg(1, Acc, Data),
                nb_setarg(1, Acc, [colour(Cat, Start, Len)|Data])
            )
        ),
        close(S)
    ),
    arg(1, Acc, Info).

nearest_term_start(Stream, StartL, TermStart) :-
    read_source_term_at_location(Stream, _, [line(StartL), error(Error)]),
    ( nonvar(Error)
    -> ( LineBack is StartL - 1,
         nearest_term_start(Stream, LineBack, TermStart) )
    ;  TermStart = StartL
    ).

file_term_colours_helper(File, line_char(StartL, _StartC), End, Info) :-
    Acc = acc([]),
    setup_call_cleanup(
        file_stream(File, S),
        ( nearest_term_start(S, StartL, TermLine),
          seek(S, 0, bof, _),
          set_stream_position(S, '$stream_position'(0,0,0,0)),
          seek_to_line(S, TermLine),
          colourise_terms_to_position(Acc, File, S, 0-0, End)
        ),
        close(S)
    ),
    arg(1, Acc, Info).

colourise_terms_to_position(Acc, File, Stream, Prev, End) :-
    once(prolog_colourise_term(
        Stream, File,
        {Acc}/[Cat, Start, Len]>>(
            arg(1, Acc, Tail),
            nb_setarg(1, Acc, [colour(Cat, Start, Len)|Tail])),
        [])),
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
    ; colourise_terms_to_position(Acc, File, Stream, Line-Char, End)
    ).
