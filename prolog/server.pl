:- module(server, []).

:- use_module(library(http/json), [json_read_dict/3,
                                   json_write_dict/3,
                                   atom_json_dict/3]).
:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(assoc), [list_to_assoc/2, get_assoc/3]).

main :-
    stream_pair(Stream, user_input, user_output),
    debug(server),
    dispatch(Stream).

dispatch(Stream) :-
    wait_for_input([Stream], _, infinite),
    handle_request(Stream),
    dispatch(Stream).

handle_request(StreamPair) :-
    phrase_from_stream(lsp_request(Req), StreamPair),
    debug(server, "Request ~w", [Req]),
    handle_msg(Req, Resp),
    Resp -> ( atom_json_dict(JsonCodes, Resp, [as(codes)]),
              length(JsonCodes, ContentLength),
              format(StreamPair, "Content-Length: ~w\r\n\r\n~s", [ContentLength, JsonCodes]) )
    ; true.

% parsing

header(Key-Value) -->
    string_without(":", KeyC), ": ", string_without("\r", ValueC),
    { string_codes(Key, KeyC), string_codes(Value, ValueC) }.

headers([Header|Headers]) -->
    header(Header), "\r\n", !,
    headers(Headers).
headers([]) --> [].

lsp_request(_{headers: Headers, body: Body}) -->
    headers(HeadersList), "\r\n",
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
      codeLensProvider: _{resolveProvider: true},
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

handle_msg(_{method: "$/cancelRequest", id: Id} :< Msg, false) :-
    debug(server, "Cancel request ~w: Msg ~w", [Id, Msg]).
handle_msg(_{method: "initialize", id: Id, params: Params} :< Msg,
          Response) :-
    _{processId: ProcId,
      capabilities: ClientCapabilities,
      rootUri: RootUri} :< Params,
    debug(server, "init ~w: ~w", [Msg, Params]),
    server_capabilities(ServerCapabilities),
    Response = _{id: Id,
                 result: _{capabilities: ServerCapabilities} }.
handle_msg(_{method: "initialized", id: Id} :< Msg, false) :-
    debug(server, "initialized ~w: ~w", [Id, Msg]).
handle_msg(_{method: "shutdown", id: Id} :< Msg, _{id: Id, result: null}) :-
    debug(server, "shut down for client ~w", [Msg]).
handle_msg(_{method: "exit"} :< Msg, false) :-
    debug(server, "now can exit", []).
handle_msg(_{id: Id} :< Msg, _{id: Id, result: true}) :-
    debug(server, "Message ~w", [Msg]).
