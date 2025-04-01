:- module(lsp_server, [main/0]).
/** <module> LSP Server

The main entry point for the Language Server implementation.

@author James Cash
*/

:- use_module(library(apply), [maplist/2]).
:- use_module(library(apply_macros)).
:- use_module(library(debug), [debug/3, debug/1]).
:- use_module(library(http/json), [atom_json_dict/3]).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source), [directory_source_files/3]).
:- use_module(library(utf8), [utf8_codes//1]).
:- use_module(library(socket), [tcp_socket/1,
                                tcp_bind/2,
                                tcp_accept/3,
                                tcp_listen/2,
                                tcp_open_socket/2]).
:- use_module(library(yall)).

:- include('path_add.pl').

:- use_module(lsp(lsp_utils)).
:- use_module(lsp(lsp_checking), [check_errors/2]).
:- use_module(lsp(lsp_parser), [lsp_request//1]).
:- use_module(lsp(lsp_changes), [handle_doc_changes/2]).
:- use_module(lsp(lsp_completion), [completions_at/3]).
:- use_module(lsp(lsp_colours), [file_colours/2,
                                 file_range_colours/4,
                                 token_types/1,
                                 token_modifiers/1]).
:- use_module(lsp(lsp_formatter), [file_format_edits/2]).
:- use_module(lsp(lsp_formatter_parser), []).

main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(report_error, true),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
    debug(server),
    start(Args).

start([stdio]) :- !,
    debug(server, "Starting stdio client", []),
    stdio_server.
start([port, Port]) :- !,
    debug(server, "Starting socket client on port ~w", [Port]),
    atom_number(Port, PortN),
    socket_server(PortN).
start(Args) :-
    debug(server, "Unknown args ~w", [Args]).


% stdio server

stdio_server :-
    current_input(In),
    current_output(Out),
    stream_pair(StreamPair, In, Out),
    handle_requests_stream(StreamPair).

% socket server
socket_server(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, StreamPair),
    stream_pair(StreamPair, AcceptFd, _),
    dispatch_socket_client(AcceptFd).

dispatch_socket_client(AcceptFd) :-
    tcp_accept(AcceptFd, Socket, Peer),
    thread_create(process_client(Socket, Peer), _, [detached(true)]),
    dispatch_socket_client(AcceptFd).

process_client(Socket, Peer) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        ( debug(server, "Connecting new client ~w", [Peer]),
          handle_requests_stream(StreamPair) ),
        close(StreamPair)).

% common stream handler

handle_requests_stream(StreamPair) :-
    stream_pair(StreamPair, In, Out),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    % handling UTF decoding in JSON parsing, but doing the auto-translation
    % causes Content-Length to be incorrect
    set_stream(In, encoding(octet)),
    set_stream(Out, encoding(utf8)),
    client_handler(A-A, In, Out).

client_handler(Extra-ExtraTail, In, Out) :-
    wait_for_input([In], _, infinite),
    fill_buffer(In),
    read_pending_codes(In, ReadCodes, Tail),
    ( Tail == []
    -> true
    ; ( ExtraTail = ReadCodes,
        handle_requests(Out, Extra, Remainder),
        client_handler(Remainder-Tail, In, Out) )
    ).

% [TODO] add multithreading? Guess that will also need a message queue
% to write to stdout
handle_requests(Out, In, Tail) :-
    handle_request(Out, In, Rest), !,
    ( var(Rest)
    -> Tail = Rest
    ; handle_requests(Out, Rest, Tail) ).
handle_requests(_, T, T).

% general handling stuff

send_message(Stream, Msg) :-
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes), width(0)]),
    phrase(utf8_codes(JsonCodes), UTF8Codes),
    length(UTF8Codes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]),
    flush_output(Stream).

handle_request(OutStream, Input, Rest) :-
    phrase(lsp_request(Req), Input, Rest),
    debug(server(high), "Request ~w", [Req.body]),
    catch(
        ( handle_msg(Req.body.method, Req.body, Resp),
          debug(server(high), "response ~w", [Resp]),
          ( is_dict(Resp) -> send_message(OutStream, Resp) ; true ) ),
        Err,
        ( debug(server, "error handling msg ~w", [Err]),
          get_dict(id, Req.body, Id),
          send_message(OutStream, _{id: Id,
                                    error: _{code: -32001,
                                             message: "server error"}})
        )).

% Handling messages

server_capabilities(_{textDocumentSync: _{openClose: true,
                                          change: 2, %incremental
                                          save: _{includeText: false},
                                          willSave: false,
                                          willSaveWaitUntil: false},
                      hoverProvider: true,
                      completionProvider: _{},
                      definitionProvider: true,
                      declarationProvider: true,
                      implementationProvider: true,
                      referencesProvider: true,
                      documentHighlightProvider: true,
                      documentSymbolProvider: true,
                      workspaceSymbolProvider: true,
                      codeActionProvider: false,
                      %% codeLensProvider: false,
                      documentFormattingProvider: true,
                      %% documentOnTypeFormattingProvider: false,
                      renameProvider: false,
                      % documentLinkProvider: false,
                      % colorProvider: true,
                      foldingRangeProvider: false,
                      % [TODO]
                      % executeCommandProvider: _{commands: ["query", "assert"]},
                      semanticTokensProvider: _{legend: _{tokenTypes: TokenTypes,
                                                          tokenModifiers: TokenModifiers},
                                                range: true,
                                                % [TODO] implement deltas
                                                full: _{delta: false}},
                      workspace: _{workspaceFolders: _{supported: true,
                                                       changeNotifications: true}}
                    }) :-
    token_types(TokenTypes),
    token_modifiers(TokenModifiers).

:- dynamic loaded_source/1.

% messages (with a response)
handle_msg("initialize", Msg,
           _{id: Id, result: _{capabilities: ServerCapabilities}}) :-
    _{id: Id, params: Params} :< Msg, !,
    ( Params.rootUri \== null
    -> ( atom_concat('file://', RootPath, Params.rootUri),
         directory_source_files(RootPath, Files, [recursive(true)]),
         maplist([F]>>assert(loaded_source(F)), Files) )
    ; true ),
    server_capabilities(ServerCapabilities).
handle_msg("shutdown", Msg, _{id: Id, result: null}) :-
    _{id: Id} :< Msg,
    debug(server, "recieved shutdown message", []).
handle_msg("textDocument/hover", Msg, _{id: Id, result: Response}) :-
    _{params: _{position: _{character: Char0, line: Line0},
                textDocument: _{uri: Doc}}, id: Id} :< Msg,
    atom_concat('file://', Path, Doc),
    Line1 is Line0 + 1,
    (  help_at_position(Path, Line1, Char0, Help)
    -> Response = _{contents: _{kind: plaintext, value: Help}}
    ;  Response = null  ).
handle_msg("textDocument/documentSymbol", Msg, _{id: Id, result: Symbols}) :-
    _{id: Id, params: _{textDocument: _{uri: Doc}}} :< Msg,
    atom_concat('file://', Path, Doc), !,
    xref_source(Path),
    findall(
        Symbol,
        ( xref_defined(Path, Goal, local(Line)),
          succ(Line, NextLine),
          succ(Line0, Line),
          functor(Goal, Name, Arity),
          format(string(GoalName), "~w/~w", [Name, Arity]),
          Symbol = _{name: GoalName,
                     kind: 12, % function
                     location:
                     _{uri: Doc,
                       range: _{start: _{line: Line0, character: 1},
                                end: _{line: NextLine, character: 0}}}}
        ),
        Symbols).
handle_msg("textDocument/definition", Msg, _{id: Id, result: Location}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Doc},
      position: _{line: Line0, character: Char0}} :< Params,
    atom_concat('file://', Path, Doc),
    succ(Line0, Line1),
    clause_in_file_at_position(Name/Arity, Path, line_char(Line1, Char0)),
    defined_at(Path, Name/Arity, Location).
handle_msg("textDocument/definition", Msg, _{id: Msg.id, result: null}) :- !.
handle_msg("textDocument/references", Msg, _{id: Id, result: Locations}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri},
      position: _{line: Line0, character: Char0}} :< Params,
    atom_concat('file://', Path, Uri),
    succ(Line0, Line1),
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    findall(
        Location,
        ( loaded_source(Doc),
          atom_concat('file://', Doc, DocUri),
          called_at(Doc, Clause, Caller, Loc),
          relative_ref_location(DocUri, Caller, Loc, Location)
        ),
        Locations), !.
handle_msg("textDocument/references", Msg, _{id: Msg.id, result: null}) :- !.
handle_msg("textDocument/completion", Msg, _{id: Id, result: Completions}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri},
      position: _{line: Line0, character: Char0}} :< Params,
    atom_concat('file://', Path, Uri),
    succ(Line0, Line1),
    completions_at(Path, line_char(Line1, Char0), Completions).
handle_msg("textDocument/formatting", Msg, _{id: Id, result: Edits}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}} :< Params,
    atom_concat('file://', Path, Uri),
    file_format_edits(Path, Edits).
handle_msg("textDocument/documentHighlight", Msg, _{id: Id, result: Locations}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri},
      position: _{line: Line0, character: Char0}} :< Params,
    atom_concat('file://', Path, Uri),
    succ(Line0, Line1),
    highlights_at_position(Path, line_char(Line1, Char0), Locations), !.
handle_msg("textDocument/documentHighlight", Msg, _{id: Id, result: null}) :-
    _{id: Id} :< Msg.
handle_msg("textDocument/semanticTokens", Msg, Response) :-
    handle_msg("textDocument/semanticTokens/full", Msg, Response).
handle_msg("textDocument/semanticTokens/full", Msg,
           _{id: Id, result: _{data: Highlights}}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}} :< Params,
    atom_concat('file://', Path, Uri), !,
    xref_source(Path),
    file_colours(Path, Highlights).
handle_msg("textDocument/semanticTokens/range", Msg,
           _{id: Id, result: _{data: Highlights}}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}, range: Range} :< Params,
    _{start: _{line: StartLine0, character: StartChar},
      end: _{line: EndLine0, character: EndChar}} :< Range,
    atom_concat('file://', Path, Uri), !,
    succ(StartLine0, StartLine), succ(EndLine0, EndLine),
    xref_source(Path),
    file_range_colours(Path,
                       line_char(StartLine, StartChar),
                       line_char(EndLine, EndChar),
                       Highlights).
% notifications (no response)
handle_msg("textDocument/didOpen", Msg, Resp) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    atom_concat('file://', Path, FileUri),
    ( loaded_source(Path) ; assertz(loaded_source(Path)) ),
    check_errors_resp(FileUri, Resp).
handle_msg("textDocument/didChange", Msg, false) :-
    _{params: _{textDocument: TextDoc,
                contentChanges: Changes}} :< Msg,
    _{uri: Uri} :< TextDoc,
    atom_concat('file://', Path, Uri),
    handle_doc_changes(Path, Changes).
handle_msg("textDocument/didSave", Msg, Resp) :-
    _{params: Params} :< Msg,
    check_errors_resp(Params.textDocument.uri, Resp).
handle_msg("textDocument/didClose", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    atom_concat('file://', Path, FileUri),
    retractall(loaded_source(Path)).
handle_msg("initialized", Msg, false) :-
    debug(server, "initialized ~w", [Msg]).
handle_msg("$/cancelRequest", Msg, false) :-
    debug(server, "Cancel request Msg ~w", [Msg]).
handle_msg("exit", _Msg, false) :-
    debug(server, "recieved exit, shutting down", []),
    halt.
% wildcard
handle_msg(_, Msg, _{id: Id, error: _{code: -32603, message: "Unimplemented"}}) :-
    _{id: Id} :< Msg, !,
    debug(server, "unknown message ~w", [Msg]).
handle_msg(_, Msg, false) :-
    debug(server, "unknown notification ~w", [Msg]).

check_errors_resp(FileUri, _{method: "textDocument/publishDiagnostics",
                             params: _{uri: FileUri, diagnostics: Errors}}) :-
    atom_concat('file://', Path, FileUri),
    check_errors(Path, Errors).
check_errors_resp(_, false) :-
    debug(server, "Failed checking errors", []).

highlights_at_position(Path, line_char(Line1, Char0), Highlights) :-
    % use the read predicate from formatter_parser + the offset <=> line number mapping
    lsp_formatter_parser:file_lines_start_end(Path, LineCharRange),
    lsp_formatter_parser:read_term_positions(Path, TermsWithPositions),
    % find the top-level term that the offset falls within
    lsp_formatter_parser:file_offset_line_position(LineCharRange, Offset, Line1, Char0),
    % find the specific sub-term containing the point
    member(TermInfo, TermsWithPositions),
    SubTermPoses = TermInfo.subterm,
    arg(1, SubTermPoses, TermFrom),
    arg(2, SubTermPoses, TermTo),
    between(TermFrom, TermTo, Offset), !,
    subterm_leaf_position(TermInfo.term, Offset, SubTermPoses, Leaf),
    % if it's the functor of a term, find all occurrences in the file
    ( Leaf = var(_)
    -> find_occurrences_of_var(Leaf, TermInfo, Matches)
    ; functor(Leaf, FuncName, Arity),
      find_occurrences_of_func(FuncName, Arity, TermsWithPositions, Matches)
    ),
    maplist(position_to_match(LineCharRange), Matches, Highlights).

position_to_match(LineCharRange, From-To, Match) :-
    lsp_formatter_parser:file_offset_line_position(LineCharRange, From, FromLine1, FromCharacter),
    lsp_formatter_parser:file_offset_line_position(LineCharRange, To, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.
position_to_match(LineCharRange, term_position(_, _, FFrom, FTo, _), Match) :-
    lsp_formatter_parser:file_offset_line_position(LineCharRange, FFrom, FromLine1, FromCharacter),
    lsp_formatter_parser:file_offset_line_position(LineCharRange, FTo, ToLine1, ToCharacter),
    succ(FromLine0, FromLine1),
    succ(ToLine0, ToLine1),
    Match = _{range: _{start: _{line: FromLine0, character: FromCharacter},
                       end: _{line: ToLine0, character: ToCharacter}}}.

find_occurrences_of_func(FuncName, Arity, TermInfos, Matches) :-
    find_occurrences_of_func(FuncName, Arity, TermInfos, Matches, []).

find_occurrences_of_func(_, _, [], Tail, Tail).
find_occurrences_of_func(FuncName, Arity, [TermInfo|Rest], Matches, Tail) :-
    find_in_term_with_positions([X]>>( functor(X, FuncName, Arity) ),
                                TermInfo.term, TermInfo.subterm, Matches, Tail0),
    find_occurrences_of_func(FuncName, Arity, Rest, Tail0, Tail).

find_occurrences_of_var(Var, TermInfo, Matches) :-
    Var = var(Name), ground(Name), % wrapped term; otherwise it's anonymous & matches nothing
    Term = TermInfo.term,
    Poses = TermInfo.subterm,
    find_in_term_with_positions([X]>>( ground(X), X = Var ), Term, Poses,
                                Matches, []).

:- meta_predicate find_in_term_with_positions(1, +, +, -, -).

find_in_term_with_positions(Needle, Term, Position, Matches, Tail) :-
    call(Needle, Term), !, % recurse?
    Matches = [Position|Tail].
find_in_term_with_positions(Needle, Term, term_position(_, _, _, _, SubPoses), Matches, Tail) :- !,
    find_in_term_subterm(Needle, Term, 1, SubPoses, Matches, Tail).
find_in_term_with_positions(Needle, Term, list_position(_, _, Elms, TailPos), Matches, Tail) :- !,
    find_in_term_list(Needle, Term, Elms, TailPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, brace_term_position(_, _, ArgPos), Matches, Tail) :- !,
    Term = {Term0},
    find_in_term_with_positions(Needle, Term0, ArgPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, parentheses_term_position(_, _, ContentPos), Matches, Tail) :- !,
    find_in_term_with_positions(Needle, Term, ContentPos, Matches, Tail).
find_in_term_with_positions(Needle, Term, dict_position(_, _, _, _, ContentPos), Matches, Tail) :- !,
    find_in_term_dict(Needle, Term, ContentPos, Matches, Tail).
find_in_term_with_positions(_, _Term, _Pos, Tail, Tail).

find_in_term_dict(_, _, [], Tail, Tail) :- !.
find_in_term_dict(Needle, Term, [Pos|Poses], Matches, Tail) :-
    key_value_position(_KVFrom, _KVTo, _SF, _ST, Key, _KeyPos, ValuePos) = Pos,
    get_dict(Key, Term, Value),
    find_in_term_with_positions(Needle, Value, ValuePos, Matches, Tail0),
    find_in_term_dict(Needle, Term, Poses, Tail0, Tail).

find_in_term_list(_, _, [], none, Tail, Tail) :- !.
find_in_term_list(Needle, TailElt, [], TailPos, Matches, Tail) :-
    find_in_term_with_positions(Needle, TailElt, TailPos, Matches, Tail).
find_in_term_list(Needle, [X|Xs], [Pos|Poses], TailPos, Matches, Tail) :-
    find_in_term_with_positions(Needle, X, Pos, Matches, Tail0),
    find_in_term_list(Needle, Xs, Poses, TailPos, Tail0, Tail).

find_in_term_subterm(_, _, _, [], Tail, Tail) :- !.
find_in_term_subterm(Needle, Term, Arg, [Position|Positions], Matches, Tail) :-
    arg(Arg, Term, SubTerm),
    NextArg is Arg + 1,
    find_in_term_with_positions(Needle, SubTerm, Position, Matches, Matches0),
    find_in_term_subterm(Needle, Term, NextArg, Positions, Matches0, Tail).

%! subterm_leaf_position(Term, Offset, SubTermPoses, Leaf) is semidet.
subterm_leaf_position(Term, Offset, From-To, Term) :- between(From, To, Offset), !.
subterm_leaf_position(Term, Offset, term_position(_, _, FFrom, FTo, _), Term) :-
    between(FFrom, FTo, Offset), !.
subterm_leaf_position(Term, Offset, term_position(From, To, _, _, Subterms), Leaf) :-
    between(From, To, Offset), !,
    functor(Term, _, Arity, _),
    between(1, Arity, Arg),
    arg(Arg, Term, Subterm),
    nth1(Arg, Subterms, SubtermPos),
    subterm_leaf_position(Subterm, Offset, SubtermPos, Leaf), !.
subterm_leaf_position(Term, Offset, list_position(From, To, Elms, _), Leaf) :-
    between(From, To, Offset), !,
    length(Elms, NElms),
    between(1, NElms, Idx),
    nth1(Idx, Term, Elm),
    nth1(Idx, Elms, ElmPos),
    subterm_leaf_position(Elm, Offset, ElmPos, Leaf), !.
subterm_leaf_position(Term, Offset, brace_term_position(From, To, BracesPos), Leaf) :-
    between(From, To, Offset), !,
    Term = {Term0},
    subterm_leaf_position(Term0, Offset, BracesPos, Leaf).
subterm_leaf_position(Term, Offset, parentheses_term_position(From, To, ContentPos), Leaf) :-
    between(From, To, Offset), !,
    subterm_leaf_position(Term, Offset, ContentPos, Leaf).
subterm_leaf_position(Term, Offset, dict_position(_From, _To, TagFrom, TagTo, _KVPoses), Leaf) :-
    between(TagFrom, TagTo, Offset), !,
    is_dict(Term, Leaf).
subterm_leaf_position(Term, Offset, dict_position(From, To, _TagFrom, _TagTo, KVPoses), Leaf) :-
    between(From, To, Offset), !,
    member(key_value_position(KVFrom, KVTo, _SF, _ST, Key, _KeyPos, ValuePos), KVPoses),
    between(KVFrom, KVTo, Offset), !,
    % keys of a literal dict aren't of interest, I think?
    get_dict(Key, Term, Value),
    subterm_leaf_position(Value, Offset, ValuePos, Leaf).
