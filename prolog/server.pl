:- module(server, [main/0]).

:- use_module(library(socket), [tcp_socket/1,
                                tcp_bind/2,
                                tcp_listen/2,
                                tcp_open_socket/3]).
:- use_module(library(http/json), [json_read_dict/3,
                                   json_write_dict/3,
                                   atom_json_dict/3]).
:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(assoc), [list_to_assoc/2, get_assoc/3]).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source), [read_source_term_at_location/3]).

main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
    debug(server),
    debug(server, "args ~w", [Args]),
    start(Args).

start([socket, PortS]) :-
    atom_number(PortS, Port), !,
    debug(server, "Starting socket client on port ~w", [Port]),
    create_server(Port).
start([stdio]) :- !,
    debug(server, "Starting stdio client", []),
    stdio_server.
start(Args) :-
    debug(server, "Unknown args ~w", [Args]).

% socket server
create_server(Port) :-
    tcp_socket(Socket),
    tcp_bind(Socket, Port),
    tcp_listen(Socket, 5),
    tcp_open_socket(Socket, AcceptPair),
    dispatch(AcceptPair).

dispatch(AcceptPair) :-
    tcp_accept(AcceptPair, Socket, _Peer),
    thread_create(process_client(Socket), _ThreadId, [detached(true)]),
    dispatch(AcceptPair).

process_client(Socket) :-
    setup_call_cleanup(
        tcp_open_socket(Socket, StreamPair),
        handle_request(StreamPair),
        close(StreamPair)
    ).

% stdio server

stdio_server :-
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    stdio_handler(A-A, In).
stdio_handler(Extra-ExtraTail, In) :-
    wait_for_input([In], _, infinite),
    fill_buffer(In),
    read_pending_codes(In, ReadCodes, Tail),
    ( Tail == [] -> true
    ; ( Tail = [],
        ExtraTail = ReadCodes,
        Codes = Extra,
        debug(server(high), "input str: ~s", [Codes]),
        open_codes_stream(Codes, Stream),
        current_output(Out),
        stream_pair(StreamPair, Stream, Out),
        handle_request(StreamPair),
        flush_output(Out),
        stdio_handler(A-A, In) )
    ; ( stdio_handler(ReadCodes-Tail, In)) ).

% general handling stuff

send_message(Stream, Msg) :-
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes)]),
    length(JsonCodes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]).

handle_request(StreamPair) :-
    phrase_from_stream(lsp_request(Req), StreamPair),
    debug(server(high), "Request ~w", [Req.body]),
    handle_msg(Req.body.method, Req.body, Resp),
    debug(server(high), "response ~w", [Resp]),
    ( is_dict(Resp) -> send_message(StreamPair, Resp) ; true ).

% parsing

header(Key-Value) -->
    string_without(":", KeyC), ": ", string_without("\r", ValueC),
    { string_codes(Key, KeyC), string_codes(Value, ValueC) }.

headers([Header]) -->
    header(Header), "\r\n\r\n", !.
headers([Header|Headers]) -->
    header(Header), "\r\n",
    headers(Headers).

lsp_request(_{headers: Headers, body: Body}) -->
    headers(HeadersList),
    { list_to_assoc(HeadersList, Headers),
      get_assoc("Content-Length", Headers, LengthS),
      number_string(Length, LengthS),
      length(JsonCodes, Length) },
    JsonCodes,
    { open_codes_stream(JsonCodes, JsonStream),
      json_read_dict(JsonStream, Body, []) }.

% Handling messages

server_capabilities(
    _{textDocumentSync: _{openClose: true,
                          change: 3, %incremental
                          willSave: true,
                          willSaveWaitUntil: false, %???
                          save: _{includeText: true}},
      hoverProvider: true, % need to refine more
      completionProvider: _{resolveProvider: true,
                            triggerCharacters: []},
      definitionProvider: true,
      declarationProvider: true,
      implementationProvider: true,
      referencesProvider: true,
      documentHighlightProvider: true,
      documentSymbolProvider: true,
      workspaceSymbolProvider: true,
      codeActionProvider: false,
      codeLensProvider: false,
      documentFormattingProvider:false,
      documentOnTypeFormattingProvider: false,
      renameProvider: false,
      documentLinkProvider: false, % ???
      colorProvider: true,
      foldingRangeProvider: false,
      executeCommandProvider: _{commands: ["query", "assert"]},
      workspace: _{workspaceFolders: _{supported: true,
                                       changeNotifications: true}}
     }
).

% messages (with a response)
handle_msg("initialize", Msg,
           _{id: Id, result: _{capabilities: ServerCapabilities} }) :-
    _{id: Id, params: Params} :< Msg, !,
    debug(server, "init ~w: ~w", [Msg, Params]),
    server_capabilities(ServerCapabilities).
handle_msg("shutdown", Msg, _{id: Id, result: null}) :-
    _{id: Id} :< Msg,
    debug(server, "recieved shutdown message", []).
% [TODO] working showing the source, now need get some more useful info
handle_msg("textDocument/hover", Msg, Response) :-
    _{params: _{position: _{character: Char, line: Line0},
                textDocument: _{uri: Doc}}, id: Id} :< Msg,
    debug(server, "Hover at ~w:~w in ~w", [Line0, Char, Doc]),
    atom_concat('file://', Path, Doc),
    Line1 is Line0 + 1,
    setup_call_cleanup(
        open(Path, read, Stream, []),
        ( read_source_term_at_location(Stream, Terms, [line(Line1)]),
          first_clause(Terms, Term),
          format(string(S), "~w", [Term]),
          Response = _{id: Id, result: _{contents: S}}
        ),
        close(Stream)
    ).
% notifications (no response)
handle_msg("textDocument/didOpen", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    atom_concat('file://', Path, FileUri),
    debug(server, "open doc ~w", [Path]),
    xref_source(Path).
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


% helpers

first_clause((Term, _), Term) :- !.
first_clause(Term, Term).
