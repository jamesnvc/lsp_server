:- module(lsp_refactor, [ rename_at_location/4 ]).

:- include('_lsp_path_add.pl').

:- use_module(library(apply_macros)).
:- use_module(library(yall)).

:- use_module(lsp(lsp_highlights)).
:- use_module(lsp(lsp_source), [loaded_source/1]).
:- use_module(lsp(lsp_reading_source)).
:- use_module(lsp(lsp_utils)).

rename_at_location(Uri, line_char(Line1, Char0), NewName, Edits) :-
    url_path(Uri, Path),
    % highlights_at_position gives us the location & span of the variables
    % using the 4-arity version instead of 3 so we can specify it should only match a variable
    lsp_highlights:highlights_at_position(Path, line_char(Line1, Char0), '$var'(_),
                                          Positions), !,
    maplist({NewName}/[P0, P1]>>put_dict(newText, P0, NewName, P1), Positions, Edits),
    atom_string(AUri, Uri), % dict key must be an atom
    dict_create(Edits, _, [AUri=Edits]).
rename_at_location(Uri, line_char(Line1, Char0), NewName, Edits) :-
    url_path(Uri, Path),
    clause_in_file_at_position(Clause, Path, line_char(Line1, Char0)),
    clause_references(Clause, RefLocations),
    clause_definitions(Clause, DefLocations),
    clause_exports(Clause, ExportLocations),
    clause_imports(Clause, ImportLocations),
    % TODO: rename pldoc as well?
    append([RefLocations, DefLocations, ExportLocations, ImportLocations], Locations),
    maplist({NewName}/[P0, P1]>>put_dict(newText, P0, NewName, P1), Locations, Edits0),
    setof(
        RefUri=DocEdits,
        setof(
            DocEdit,
            Edit^(
                member(Edit, Edits0),
                del_dict(uri, Edit, RefUri, DocEdit)
            ),
            DocEdits
        ),
        EditsList
    ),
    dict_create(Edits, @, EditsList).

clause_references(Clause, LLocations) :-
    findall(
        Locations,
        ( loaded_source(Doc),
          url_path(DocUri, Doc),
          called_at(Doc, Clause, Locs0),
          % handle the case where Caller = imported(Path)?
          maplist({DocUri}/[D0, D]>>put_dict(uri, D0, DocUri, D), Locs0, Locations)
        ),
        LLocations0),
    append(LLocations0, LLocations).

clause_definitions(Clause, Locations) :-
    name_callable(Clause, Callable),
    findall(
        Location,
        ( loaded_source(Doc),
          xref_source(Doc),
          xref_defined(Doc, Callable, local(_)),
          definition_location_in(Doc, Callable, Location)
        ),
        Locations).

definition_location_in(Doc, Callable, Location) :-
    file_lines_start_end(Doc, LineCharRange),
    read_term_positions(Doc, TermPositions),
    url_path(DocUri, Doc),
    member(TermPos, TermPositions),
    ( ( get_dict(term, TermPos, (Callable :- _)),
        get_dict(subterm, TermPos, SubTermPos0),
        SubTermPos0 = term_position(_, _, _, _, [term_position(_, _, FFrom, FTo, _)|_])
      ) ; (
          get_dict(term, TermPos, Callable),
          get_dict(subterm, TermPos, SubTermPos0),
          SubTermPos0 = term_position(_, _, FFrom, FTo, _)
      ) ),
    file_offset_line_position(LineCharRange, FFrom, StartLine1, StartChar),
    succ(StartLine0, StartLine1),
    file_offset_line_position(LineCharRange, FTo, EndLine1, EndChar),
    succ(EndLine0, EndLine1),
    Location = @{uri: DocUri,
                 range: @{start: @{line: StartLine0, character: StartChar},
                          end: @{line: EndLine0, character: EndChar}}}.

clause_exports(Clause, Locations) :-
    name_callable(Clause, Callable),
    findall(
        Location,
        ( loaded_source(Doc),
          xref_source(Doc),
          xref_defined(Doc, Callable, local(_)),
          export_location_in(Doc, Clause, Location)
        ),
        Locations).

export_location_in(Doc, Clause, Location) :-
    file_lines_start_end(Doc, LineCharRange),
    read_term_positions(Doc, TermPositions),
    member(TermPos, TermPositions),
    get_dict(term, TermPos, (:- module(_, Exports))),
    member(Clause, Exports), !,
    find_in_term_with_positions(
        {Clause}/[X, _]>>( nonvar(X), X = Clause ),
        TermPos.term, TermPos.subterm,
        Matches, []),
    % matches are for the '/'(Name, Arity) term but we just want to change Name
    maplist([found_at(Name/_Arity, term_position(_F, _T, _FF, _FT, [Start-End|_]))]>>(
                position_to_match(LineCharRange, found_at(Name, Start-End))
            ),
            Matches, Locations),
    member(Location0, Locations),
    url_path(Uri, Doc),
    put_dict(uri, Location0, Uri, Location).

clause_imports(Clause, Locations) :-
    name_callable(Clause, Callable),
    findall(
        Location,
        ( loaded_source(Doc),
          xref_source(Doc),
          % Use xref_defined with imported or called?
          xref_defined(Doc, Callable, imported(_)),
          import_location_in(Doc, Clause, Location)
        ),
        Locations).

import_location_in(Doc, Clause, Location) :-
    file_lines_start_end(Doc, LineCharRange),
    read_term_positions(Doc, TermPositions),
    member(TermPos, TermPositions),
    get_dict(term, TermPos, (:- use_module(_, Imports))),
    member(Clause, Imports), !,
    find_in_term_with_positions(
        {Clause}/[X, _]>>( nonvar(X), X = Clause ),
        TermPos.term, TermPos.subterm,
        Matches, []),
    % matches are for the '/'(Name, Arity) term but we just want to change Name
    maplist([found_at(Name/_Arity, term_position(_F, _T, _FF, _FT, [Start-End|_]))]>>(
                position_to_match(LineCharRange, found_at(Name, Start-End))
            ),
            Matches, Locations),
    member(Location0, Locations),
    url_path(Uri, Doc),
    put_dict(uri, Location0, Uri, Location).
