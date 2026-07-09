:- module(server_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_server)).

:- begin_tests(server).

test('Handle initialize with missing capabilities.general',
     [ true(ClientEncoding-HoverFormat =@= 'utf-16'-plaintext) ]) :-
    lsp_server:handle_msg("initialize", #{id: 1, params: #{rootUri: null}}, _),
    lsp_server:client_encoding(ClientEncoding),
    lsp_server:client_hover_format(HoverFormat).

test('Can specifiy encodings',
     [ true(ClientEncoding-HoverFormat =@= 'utf-32'-plaintext) ]) :-
    lsp_server:handle_msg("initialize",
                          #{id: 1,
                            params: #{rootUri: null,
                                      capabilities:
                                      #{general:
                                        #{positionEncodings: ["utf-32", "utf-8", "utf-16"]}}
                          }}, _),
    lsp_server:client_encoding(ClientEncoding),
    lsp_server:client_hover_format(HoverFormat).


:- end_tests(server).
