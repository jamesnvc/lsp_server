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
:- use_module(library(help), [help_html/3, help_objects/3]).
:- use_module(library(lynx/html_text), [html_text/1]).
:- use_module(library(utf8), [utf8_codes//1]).

main :-
    set_prolog_flag(debug_on_error, false),
    set_prolog_flag(report_error, true),
    set_prolog_flag(toplevel_prompt, ''),
    current_prolog_flag(argv, Args),
    debug(server),
    debug(server, "args ~w", [Args]),
    start(Args).

start([stdio]) :- !,
    debug(server, "Starting stdio client", []),
    stdio_server.
start(Args) :-
    debug(server, "Unknown args ~w", [Args]).


% stdio server

stdio_server :-
    current_input(In),
    set_stream(In, buffer(full)),
    set_stream(In, newline(posix)),
    set_stream(In, tty(false)),
    set_stream(In, representation_errors(error)),
    current_output(Out),
    set_stream(Out, encoding(utf8)),
    stdio_handler(A-A, In).
% [TODO] add multithreading? Guess that will also need a message queue
% to write to stdout
stdio_handler(Extra-ExtraTail, In) :-
    wait_for_input([In], _, infinite),
    fill_buffer(In),
    read_pending_codes(In, ReadCodes, Tail),
    ( Tail == []
    -> true
    ; ( current_output(Out),
        ExtraTail = ReadCodes,
        handle_requests(Out, Extra, Remainder),
        stdio_handler(Remainder-Tail, In) )
    ).

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

% parsing

header(Key-Value) -->
    string_without(":", KeyC), ": ", string_without("\r", ValueC),
    { string_codes(Key, KeyC), string_codes(Value, ValueC) }.

headers([Header]) -->
    header(Header), "\r\n\r\n", !.
headers([Header|Headers]) -->
    header(Header), "\r\n",
    headers(Headers).

json_chars(0, []) --> [].
json_chars(N, [C|Cs]) --> [C], { succ(Nn, N) }, json_chars(Nn, Cs).

lsp_request(_{headers: Headers, body: Body}) -->
    headers(HeadersList),
    { list_to_assoc(HeadersList, Headers),
      get_assoc("Content-Length", Headers, LengthS),
      number_string(Length, LengthS) },
    json_chars(Length, JsonCodes),
    { ground(JsonCodes),
      open_codes_stream(JsonCodes, JsonStream),
      json_read_dict(JsonStream, Body, []) }.

% Handling messages

server_capabilities(
    _{textDocumentSync: _{openClose: true,
                          change: 3, %incremental
                          willSave: false,
                          willSaveWaitUntil: false %???
                          },
      hoverProvider: true, % need to refine more
      %% completionProvider: false,
      definitionProvider: true,
      declarationProvider: true,
      implementationProvider: true,
      referencesProvider: true,
      documentHighlightProvider: false,
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
handle_msg("textDocument/hover", Msg, Response) :-
    _{params: _{position: _{character: Char0, line: Line0},
                textDocument: _{uri: Doc}}, id: Id} :< Msg,
    debug(server(hover), "Hover at ~w:~w in ~w", [Line0, Char0, Doc]),
    atom_concat('file://', Path, Doc),
    Line1 is Line0 + 1,
    catch(help_at_position(Path, Line1, Char0, Id, Response),
          Err, (debug(server(hover), "error getting help ~w", [Err]),
                Response = _{id: Id, result: null})).
handle_msg("textDocument/documentSymbol", Msg, _{id: Id, result: Symbols}) :-
    _{id: Id, params: _{textDocument: _{uri: Doc}}} :< Msg,
    atom_concat('file://', Path, Doc), !,
    xref_source(Path),
    findall(Goal-Line, xref_defined(Path, Goal, local(Line)), GoalLines),
    maplist({Doc}/[Goal-Line,
                   _{name: GoalName, kind: 12, % function
                     location: _{uri: Doc,
                                 range: _{start: _{line: Line0, character: 1},
                                          end: _{line: NextLine, character: 0}}}}
                  ]>>( succ(Line, NextLine),
                       succ(Line0, Line),
                       functor(Goal, Name, Arity),
                       format(string(GoalName), "~w/~w", [Name, Arity]) ),
            GoalLines,
            Symbols).
handle_msg("textDocument/definition", Msg, _{id: Id, result: Location}) :-
    _{id: Id, params: _{textDocument: _{uri: Doc},
                        position: _{line: Line0, character: Char0}}} :< Msg,
    atom_concat('file://', Path, Doc),
    succ(Line0, Line1),
    setup_call_cleanup(
        open(Path, read, Stream, []),
        clause_at_position(Stream, Name/Arity, line_char(Line1, Char0)),
        close(Stream)
    ),
    name_callable(Name/Arity, Callable),
    xref_source(Path),
    xref_defined(Path, Callable, Ref),
    relative_ref_location(Doc, Callable, Ref, Location).
handle_msg("textDocument/definition", Msg, _{id: Msg.id, result: null}) :- !.
handle_msg("textDocument/references", Msg, _{id: Id, result: Locations}) :-
    debug(server, "searching for refs ~w", [Msg]),
    _{id: Id, params: _{textDocument: _{uri: Doc},
                        context: _,
                        position: _{line: Line0, character: Char0}}} :< Msg,
    atom_concat('file://', Path, Doc),
    succ(Line0, Line1),
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    name_callable(Clause, Callable),
    xref_source(Path),
    findall(By-Ref,
            (xref_called(Path, Callable, By),
             xref_defined(Path, By, Ref)),
            Sources),
    debug(server, "refs for ~w: ~w", [Clause, Sources]),
    % [TODO] xref just gives the predicates that call; need to find the actual line
    maplist({Doc}/[Caller-Loc, Location]>>relative_ref_location(Doc, Caller, Loc, Location),
            Sources,
            Locations), !.
handle_msg("textDocument/references", Msg, _{id: Msg.id, result: null}) :- !.
% notifications (no response)
handle_msg("textDocument/didOpen", Msg, false) :-
    _{params: _{textDocument: TextDoc}} :< Msg,
    _{uri: FileUri} :< TextDoc,
    atom_concat('file://', Path, FileUri),
    debug(server, "open doc ~w", [Path]),
    xref_source(Path).
handle_msg("textDocument/didSave", _, false).
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

name_callable(Name/Arity, Callable) :-
    length(FakeArgs, Arity),
    Callable =.. [Name|FakeArgs], !.

relative_ref_location(Here, _, local(Line1),
                      _{uri: Here, range: _{start: _{line: Line0, character: 1},
                                            end: _{line: NextLine, character: 0}}}) :-
    !, succ(Line0, Line1), succ(Line1, NextLine).
relative_ref_location(_, Goal, imported(Path), Location) :-
    atom_concat('file://', Path, ThereUri),
    xref_source(Path),
    xref_defined(Path, Goal, Loc),
    relative_ref_location(ThereUri, Goal, Loc, Location).

help_at_position(Path, Line1, Char0, Id, _{id: Id, result: _{contents: S}}) :-
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    predicate_help(Path, Clause, S), !.
help_at_position(_, _, _, Id, _{id: Id, result: null}).

predicate_help(_, Pred, Help) :-
    nonvar(Pred),
    help_objects(Pred, exact, Matches), !,
    catch(help_html(Matches, exact-exact, HtmlDoc), _, fail),
    setup_call_cleanup(open_string(HtmlDoc, In),
                       load_html(stream(In), Dom, []),
                       close(In)),
    with_output_to(string(Help), html_text(Dom)).

first_clause((Term, _), Term) :- !.
first_clause(Term, Term).

linechar_offset(Stream, line_char(Line1, Char1), Offset) :-
    seek(Stream, 0, bof, _),
    seek_to_line(Stream, Line1),
    seek(Stream, Char1, current, Offset).

seek_to_line(Stream, N) :-
    N > 1, !,
    skip(Stream, 0'\n),
    NN is N - 1,
    seek_to_line(Stream, NN).
seek_to_line(_, _).

clause_in_file_at_position(Clause, Path, Position) :-
    setup_call_cleanup(
        open(Path, read, Stream, []),
        clause_at_position(Stream, Clause, Position),
        close(Stream)
    ).

clause_at_position(Stream, Clause, Start) :-
    linechar_offset(Stream, Start, Offset), !,
    clause_at_position(Stream, Clause, Start, Offset).
clause_at_position(Stream, Clause, line_char(Line1, Char), Here) :-
    read_source_term_at_location(Stream, Terms, [line(Line1),
                                                 subterm_positions(SubPos),
                                                 error(Error)]),
    extract_clause_at_position(Stream, Terms, line_char(Line1, Char), Here,
                               SubPos, Error, Clause).

extract_clause_at_position(Stream, _, line_char(Line1, Char), Here, _, Error, Clause) :-
    nonvar(Error), !, Line1 > 1,
    LineBack is Line1 - 1,
    clause_at_position(Stream, Clause, line_char(LineBack, Char), Here).
extract_clause_at_position(_, Terms, _, Here, SubPos, _, Clause) :-
    find_clause(Terms, Here, SubPos, Clause).

find_clause(Term, Offset, term_position(_, _, FF, FT, _), Name/Arity) :-
    between(FF, FT, Offset), !,
    functor(Term, Name, Arity).
find_clause(Term, Offset, term_position(F, T, _, _, SubPoses), Clause) :-
    between(F, T, Offset), !,
    Term =.. [_|SubTerms],
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos), !,
    find_clause(SubTerm, Offset, SubPos, Clause).
find_clause(Term, Offset, term_position(F, T, _, _, _), Name/Arity) :-
    between(F, T, Offset), !,
    functor(Term, Name, Arity).
find_clause(Term, Offset, paretheses_term_position(F, T, SubPoses), Clause) :-
    between(F, T, Offset),
    parens_list(Term, SubTerms),
    find_containing_term(Offset, SubTerms, SubPoses, SubTerm, SubPos),
    find_clause(SubTerm, Offset, SubPos, Clause).
find_clause({SubTerm}, Offset, brace_term_position(F, T, SubPos), Clause) :-
    between(F, T, Offset),
    find_clause(SubTerm, Offset, SubPos, Clause).

find_containing_term(Offset, [Term|_], [P|_], Term, P) :-
    P = term_position(F, T, _, _, _),
    between(F, T, Offset), !.
find_containing_term(Offset, [Term|_], [PP|_], Term, P) :-
    PP = parentheses_term_position(F, T, P),
    between(F, T, Offset), !.
find_containing_term(Offset, [BTerm|_], [BP|_], Term, P) :-
    BP = brace_term_position(F, T, P),
    {Term} = BTerm,
    between(F, T, Offset).
find_containing_term(Offset, [_|Ts], [_|Ps], T, P) :-
    find_containing_term(Offset, Ts, Ps, T, P).

%! parens_list(+Term:term, -List:list) is det.
%
%  True when =Term= is a parethesized term (i.e. a term with the
%  functor ',') and =List= is the equivalent list.
%  e.g. =parens_list((foo, bar(baz), quux), [foo, bar(baz), quux]).=
parens_list(','(A, RstP), [A|RstL]) :-
    !, parens_list(RstP, RstL).
parens_list(A, [A]).
