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

main :-
    current_prolog_flag(argv, Args),
    debug(server),
    debug(server, "args ~w", [Args]),
    start(Args).

start([socket, PortS]) :-
    atom_number(PortS, Port),
    create_server(Port).
start([stdio]) :-
    debug(server, "Starting stdio client", []).
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

% general handling stuff

send_message(Stream, Msg) :-
    put_dict(jsonrpc, Msg, "2.0", VersionedMsg),
    atom_json_dict(JsonCodes, VersionedMsg, [as(codes)]),
    length(JsonCodes, ContentLength),
    format(Stream, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]).

handle_request(StreamPair) :-
    phrase_from_stream(lsp_request(Req), StreamPair),
    debug(server, "Request ~w", [Req.body]),
    handle_msg(Req.body, Resp),
    debug(server, "response ~w", [Resp]),
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
      hoverProvider: true,
      completionProvider: _{resolveProvider: true,
                            triggerCharacters: []},
      definitionProvider: true,
      implementationProvider: _{documentSelector: [_{language: "prolog"}],
                                id: prologImplementation},
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
      colorProvider: _{documentSelector: [_{language: "prolog"}],
                       id: prologColor},
      foldingRangeProvider: false,
      executeCommandProvider: _{commands: ["query", "assert"]},
      workspace: _{workspaceFolders: _{supported: true,
                                       changeNotifications: true}}
     }
).

handle_msg(Msg, false) :-
    _{method: "$/cancelRequest", id: Id} :< Msg,
    debug(server, "Cancel request ~w: Msg ~w", [Id, Msg]).
handle_msg(Msg, Response) :-
    _{method: "initialize", id: Id, params: Params} :< Msg,
    _{processId: ProcId,
      capabilities: Capabilities,
      rootUri: RootUri} :< Params,
    debug(server, "init ~w: ~w", [Msg, Params]),
    server_capabilities(ServerCapabilities),
    Response = _{id: Id,
                 result: _{capabilities: ServerCapabilities} }.
handle_msg(Msg, _{id: Id, result: true}) :-
    _{method: "initialized", id: Id} :< Msg,
    debug(server, "initialized ~w: ~w", [Id, Msg]).
handle_msg(Msg, _{id: Id, result: true}) :-
    _{id: Id} :< Msg,
    debug(server, "Message ~w", [Msg]).
