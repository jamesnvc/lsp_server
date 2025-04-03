:- module(server_t, []).

:- use_module(library(plunit)).

:- include('../prolog/path_add.pl').
:- use_module(lsp(lsp_parser)).

:- begin_tests(parsing).

test('Parsing content') :-
 S = `Content-Length: 100\r\n\r\n{"jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/didOpen",
  "params": {
    "thing": 1
  }
}`, phrase(lsp_request(Req), S),
 _{headers: _,
   body: _{jsonrpc: "2.0",
           id: 1,
           method: "textDocument/didOpen",
           params: _{thing: 1}}} :< Req.


:- end_tests(parsing).
