:- module(refactor_t, []).

:- use_module(library(plunit)).

:- include('../prolog/_lsp_path_add.pl').
:- use_module(lsp(lsp_refactor)).
:- use_module(lsp(lsp_source), [loaded_source/1]).
:- use_module(lsp(lsp_utils), [url_path/2]).

:- begin_tests(refactor).

test('Renaming predicate across project',
     % this is annoyingly fragile to depend on the whole project...
     % but also annoying to set up a real test for this
     % so...keep it updated, I guess?
     [ true( EditsPairs =@= [ChangesUri-[@{newText:handle_document_changes,
                                           range: @{end: @{character:18, line:18},
                                                    start: @{character:0, line:18}}},
                                         @{newText:handle_document_changes,
                                           range: @{end: @{character:18, line:19},
                                                    start: @{character:0, line:19}}},
                                         @{newText:handle_document_changes,
                                           range: @{end: @{character:22, line:21},
                                                    start: @{character:4, line:21}}},
                                         @{newText:handle_document_changes,
                                           range: @{end: @{character:42, line:0},
                                                    start: @{character:24, line:0}}}],
                             ServerUri-[@{newText:handle_document_changes,
                                          range: @{end: @{character:22, line:366},
                                                   start: @{character:4, line:366}}},
                                        @{newText:handle_document_changes,
                                          range: @{end: @{character:51, line:29},
                                                   start: @{character:33, line:29}}}]] ) ]) :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(ProjectDir, ThisFile, '../prolog'),
    directory_file_path(ProjectDir, 'lsp_server.pl', ServerFile),
    url_path(ServerUri, ServerFile),
    directory_file_path(ProjectDir, 'lsp_changes.pl', ChangesFile),
    url_path(ChangesUri, ChangesFile),
    directory_source_files(ProjectDir, ProjectFiles, [recursive(true), if(true)]),
    forall(member(F, ProjectFiles), assertz(lsp_source:loaded_source(F))),
    % fragile here - change line/char to be on ⤵ 'handle_doc_changes' in handle_msg/3 clause for didChange
    rename_at_location(ServerUri, line_char(367, 11), 'handle_document_changes', Edits),
    dict_pairs(Edits, _, EditsPairs).

test('Renaming a variable in predicate',
     [ true( EditsPairs =@= [ServerUri-[@{newText:'Message',
                                          range: @{end: @{character:37, line:321},
                                                   start: @{character:34, line:321}}},
                                        @{newText:'Message',
                                          range: @{end: @{character:36, line:322},
                                                   start: @{character:33, line:322}}}]]) ])  :-
    context_module(ThisModule),
    module_property(ThisModule, file(ThisFile)),
    relative_file_name(ProjectDir, ThisFile, '../prolog'),
    directory_file_path(ProjectDir, 'lsp_server.pl', ServerFile),
    url_path(ServerUri, ServerFile),
    % fragile here - change line/char to be on ⤵ 'Msg' in handle_msg/3 clause for textDocument/rename
    rename_at_location(ServerUri, line_char(322, 34), 'Message', Edits),
    dict_pairs(Edits, _, EditsPairs).


:- end_tests(refactor).
