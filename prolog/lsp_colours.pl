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
:- use_module(library(readutil), [read_file_to_codes/3]).
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
%  True when Colours is a list of colour information
%  corresponding to the file File.
file_colours(File, Tuples) :-
    file_colours_helper(File, Colours0),
    sort(2, @=<, Colours0, Colours),
    flatten_colour_terms(File, Colours, Tuples).

%! file_range_colours(+File, +Start, +End, -Colours) is det.
%
%  True when Colours is a list of colour information corresponding
%  to file File covering the terms between Start and End. Note
%  that it may go beyond either bound.
file_range_colours(File, Start, End, Tuples) :-
    file_term_colours_helper(File, Start, End, Colours0),
    sort(2, @=<, Colours0, Colours),
    flatten_colour_terms(File, Colours, Tuples).

file_stream(File, S) :-
    ( doc_text(File, Changes)
    -> open_string(Changes, S)
    ;  open(File, read, S) ),
    set_stream(S, newline(posix)).

%! flatten_colour_terms(+File, +ColourTerms, -Nums) is det.
%
%  Convert the list of ColourTerms like =colour(Category, Start, Length)=
%  to a flat list of numbers Nums in the format that LSP expects.
%
%  Offsets coming from prolog_colourise_stream/3 are codepoint counts;
%  converting them to (line, column) pairs is done via a precomputed
%  list of line-start offsets rather than by repositioning the stream.
%  seek/4 on text streams is only guaranteed for fixed-size encodings,
%  so any backward seek on UTF-8 can land inside a multibyte sequence
%  and corrupt the character count.
%
%  @see https://microsoft.github.io/language-server-protocol/specifications/specification-3-16/#textDocument_semanticTokens
flatten_colour_terms(File, ColourTerms, Nums) :-
    token_types_dict(TokenDict),
    source_codes(File, Codes),
    line_start_offsets(Codes, Offsets),
    InitialPosition = position(Offsets, 1, 0),
    InitialDeltaRef = delta_ref(1, 0),
    phrase(emit_semantic_tokens(TokenDict, InitialPosition, InitialDeltaRef),
           ColourTerms,
           Nums).

%! source_codes(+File, -Codes) is det.
%
%  Codes is the character-code list of the source being highlighted,
%  taken from the in-memory edit buffer if the client has sent changes,
%  otherwise read from disk. Newlines are preserved as-is so that the
%  offsets match what prolog_colourise_stream/3 sees.
source_codes(File, Codes) :-
    ( doc_text(File, Buffer)
    -> ensure_codes(Buffer, Codes)
    ;  read_file_to_codes(File, Codes, [newline(posix)])
    ).

ensure_codes(Codes, Codes) :- is_list(Codes), !.
ensure_codes(String, Codes) :- string_codes(String, Codes).

%! line_start_offsets(+Codes, -Offsets) is det.
%
%  Offsets is the ordered list of codepoint offsets at which line 2,
%  line 3, ... begin. Line 1 implicitly begins at offset 0 and is not
%  represented.
line_start_offsets(Codes, Offsets) :-
    line_start_offsets_(Codes, 0, Offsets).

line_start_offsets_([], _, []).
line_start_offsets_([0'\n|Rest], Offset, [NextLineStart|More]) :- !,
    NextLineStart is Offset + 1,
    line_start_offsets_(Rest, NextLineStart, More).
line_start_offsets_([_|Rest], Offset, More) :-
    NextOffset is Offset + 1,
    line_start_offsets_(Rest, NextOffset, More).

%! emit_semantic_tokens(+TypeDict, +Position, +DeltaRef)//
%
%  Emit the LSP semantic-token 5-tuple for each colour term in the
%  input. Position is the line-offset lookup cursor advancing forward
%  through the source; DeltaRef is the reference point for delta
%  encoding.
emit_semantic_tokens(TypeDict, Position, DeltaRef), Tuple -->
    [colour(Category, Offset, Length)], !,
    { locate(Position, Offset, PositionAfter, Line, Column),
      ( lsp_token_of(Category, TypeDict, TypeCode, Modifiers)
      -> tuple_at(DeltaRef, Line, Column, Length, TypeCode, Modifiers, Tuple),
         DeltaRefAfter = delta_ref(Line, Column)
      ;  Tuple = [],
         DeltaRefAfter = DeltaRef
      )
    },
    emit_semantic_tokens(TypeDict, PositionAfter, DeltaRefAfter).
emit_semantic_tokens(_, _, _) --> [].

%! locate(+Position, +Offset, -PositionAfter, -Line, -Column) is det.
%
%  Advance Position past any line boundaries that precede Offset.
%  PositionAfter's line and line-start are where Offset falls; Line and
%  Column (both 1-indexed / 0-indexed respectively) describe that
%  location.
locate(position([NextLineStart|Rest], Line, _), Offset, PositionAfter, FinalLine, Column) :-
    Offset >= NextLineStart, !,
    NextLine is Line + 1,
    locate(position(Rest, NextLine, NextLineStart), Offset, PositionAfter, FinalLine, Column).
locate(position(Pending, Line, LineStart), Offset,
       position(Pending, Line, LineStart), Line, Column) :-
    Column is Offset - LineStart.

%! lsp_token_of(+ColourCategory, +TypeDict, -TypeCode, -Modifiers) is semidet.
%
%  Fails when the colour category has no LSP token mapping, which
%  causes the current term to be skipped without emitting a tuple.
lsp_token_of(Category, TypeDict, TypeCode, Modifiers) :-
    colour_type(Category, TokenName, Modifiers),
    get_dict(TokenName, TypeDict, TypeCode).

%! tuple_at(+DeltaRef, +Line, +Column, +Length, +TypeCode, +Modifiers, -Tuple) is det.
%
%  Tuple is the LSP 5-number encoding [DeltaLine, DeltaStart, Length,
%  TypeCode, ModifierMask] for a token at (Line, Column), with deltas
%  taken against DeltaRef.
tuple_at(delta_ref(RefLine, RefColumn), Line, Column, Length, TypeCode, Modifiers,
         [DeltaLine, DeltaStart, Length, TypeCode, ModifierMask]) :-
    ( Line == RefLine
    -> DeltaLine = 0, DeltaStart is Column - RefColumn
    ;  DeltaLine is Line - RefLine, DeltaStart = Column
    ),
    mods_mask(Modifiers, ModifierMask).

colour_type(directive,                namespace, []).

colour_type(head(_,              _),        function,  [declaration]).
% colour_type(head_term(_,              _),        function,  [declaration]).
colour_type(neck(directive),          operator,  [declaration]).
colour_type(neck(':-'),               operator,  [declaration]).
colour_type(neck(clause),             operator,  [definition]).
colour_type(neck(grammar_rule),       operator,  [definition]).
colour_type(goal(built_in,       A),        macro,     []) :- atom(A), !.
colour_type(goal(built_in,       _),        function,  [defaultLibrary]).
colour_type(goal(undefined,      _),        function,  []).
colour_type(goal(imported(_),    _),        function,  []).
colour_type(goal(local(_),       _),        function,  []).
colour_type(goal(extern(_,_),    _),        function,  []).
colour_type(goal(recursion,      _),        member,    []).
colour_type(goal(('dynamic'(_)), _),        member, []).
% colour_type(goal_term(built_in,       A),        macro,     []) :- atom(A), !.
% colour_type(goal_term(built_in,       _),        function,  [defaultLibrary]).
% colour_type(goal_term(undefined,      _),        function,  []).
% colour_type(goal_term(imported(_),    _),        function,  []).
% colour_type(goal_term(local(_),       _),        function,  []).
% colour_type(goal_term(extern(_,_),    _),        function,  []).
% colour_type(goal_term(recursion,      _),        member,    []).
% colour_type(goal_term(('dynamic'(_)), _),        member, []).
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
%  Use prolog_colourise_stream/3 to accumulate a list of colour
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
