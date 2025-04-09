:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   ( user:file_search_path(lsp_project, _)
   -> true
   ;  asserta(user:file_search_path(lsp, Dir)) ).
