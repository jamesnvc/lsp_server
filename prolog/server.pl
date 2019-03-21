:- module(server, []).

:- use_module(library(socket), [tcp_socket/1,
                                tcp_bind/2,
                                tcp_listen/2,
                                tcp_open_socket/3]).
:- use_module(library(http/json), [json_read_dict/3,
                                   json_write_dict/3]).
:- use_module(library(dcg/basics), [remainder//1,
                                    string_without//2]).

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

handle_request(StreamPair) :-
    phrase_from_stream(lsp_request(Req), StreamPair),
    debug(server, "Request ~w", [Req]).

% parsing

header(Key-Value) -->
    string_without(":", KeyC), ": ", string_without("\r", ValueC),
    { string_codes(Key, KeyC), string_codes(Value, ValueC) }.

headers([Header|Headers]) -->
    header(Header), "\r\n", !,
    headers(Headers).
headers([]) --> [].

lsp_request(_{headers: Headers, body: Body}) -->
    headers(Headers), "\r\n",
    remainder(JsonCodes),
    { open_codes_stream(JsonCodes, JsonStream),
      json_read_dict(JsonStream, Body, []) }.
