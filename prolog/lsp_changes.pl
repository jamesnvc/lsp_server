:- module(lsp_changes, [handle_doc_changes/2,
                        doc_text_fallback/2]).

:- use_module(library(readutil), [read_file_to_codes/3]).

:- dynamic doc_text/2.

handle_doc_changes(_, []) :- !.
handle_doc_changes(Path, [Change|Changes]) :-
    handle_doc_change(Path, Change),
    handle_doc_changes(Path, Changes).

handle_doc_change(Path, Change) :-
    _{range: _{start: _{line: StartLine, character: StartChar},
               end:   _{line: _EndLine0,   character: _EndChar}},
      rangeLength: ReplaceLen, text: Text} :< Change,
    !,
    atom_codes(Text, ChangeCodes),
    doc_text_fallback(Path, OrigCodes),
    replace_codes(OrigCodes, StartLine, StartChar, ReplaceLen, ChangeCodes,
                  NewText),
    retractall(doc_text(Path, _)),
    assertz(doc_text(Path, NewText)).
handle_doc_change(Path, Change) :-
    retractall(doc_text(Path, _)),
    atom_codes(Change.text, TextCodes),
    assertz(doc_text(Path, TextCodes)).

doc_text_fallback(Path, Text) :-
    doc_text(Path, Text), !.
doc_text_fallback(Path, Text) :-
    read_file_to_codes(Path, Text, []),
    assertz(doc_text(Path, Text)).

%! replace_codes(Text, StartLine, StartChar, ReplaceLen, ReplaceText, -NewText) is det.
replace_codes(Text, StartLine, StartChar, ReplaceLen, ReplaceText, NewText) :-
    phrase(replace(StartLine, StartChar, ReplaceLen, ReplaceText),
           Text,
           NewText).

replace(0, 0, 0, NewText), NewText --> !, [].
replace(0, 0, Skip, NewText) -->
    !, skip(Skip),
    replace(0, 0, 0, NewText).
replace(0, Chars, Skip, NewText), Take -->
    { length(Take, Chars) },
    Take, !,
    replace(0, 0, Skip, NewText).
replace(Lines1, Chars, Skip, NewText), Line -->
    line(Line), !,
    { succ(Lines0, Lines1) },
    replace(Lines0, Chars, Skip, NewText).

skip(0) --> !, [].
skip(N) --> [_], { succ(N0, N) }, skip(N0).

line([0'\n]) --> [0'\n], !.
line([C|Cs]) --> [C], line(Cs).
