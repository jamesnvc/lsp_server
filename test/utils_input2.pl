:- module(lsp_parser, [lsp_request//1]).
/** <module> LSP Parser

Module for parsing the body & headers from an LSP client.

@author James Cash
*/

:- use_module(library(assoc), [list_to_assoc/2, get_assoc/3]).
:- use_module(library(codesio), [open_codes_stream/2]).
:- use_module(library(dcg/basics), [string_without//2]).
:- use_module(library(http/json), [json_read_dict/3]).

header(Key-Value) -->
    string_without(":", KeyC), ": ", string_without("\r", ValueC),
    { string_codes(Key, KeyC), string_codes(Value, ValueC) }.

headers([Header]) -->
    header(Header), "\r\n\r\n", !.
headers([Header|Headers]) -->
    header(Header), "\r\n",
    headers(Headers).

other_thing(H) :- header(H, _, _).

lsp_request(_{headers: Headers, body: Body}) -->
    headers(HeadersList),
    { list_to_assoc(HeadersList, Headers),
      get_assoc("Content-Length", Headers, LengthS),
      number_string(Length, LengthS),
      length(JsonCodes, Length) },
    JsonCodes,
    { ground(JsonCodes),
      open_codes_stream(JsonCodes, JsonStream),
      json_read_dict(JsonStream, Body, []) }.
