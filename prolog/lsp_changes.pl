:- module(lsp_changes, [handle_doc_changes/2,
                        doc_text_fallback/2,
                        doc_text/2]).
/** <module> LSP changes

Module for tracking edits to the source, in order to be able to act on
the code as it is in the editor buffer, before saving.

@author James Cash
*/

:- use_module(library(readutil), [read_file_to_codes/3]).

:- dynamic doc_text/2.

%! handle_doc_changes(+File:atom, +Changes:list) is det.
%
%  Track =Changes= to the file =File=.
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
    _{range: _{start: _{line: StartLine, character: StartChar},
               end:   _{line: EndLine,   character: EndChar}},
      text: Text} :< Change,
    !,
    atom_codes(Text, ChangeCodes),
    doc_text_fallback(Path, OrigCodes),
    replace_codes_range(OrigCodes, StartLine, StartChar, EndLine, EndChar, ChangeCodes,
                        NewText),
    retractall(doc_text(Path, _)),
    assertz(doc_text(Path, NewText)).
handle_doc_change(Path, Change) :-
    retractall(doc_text(Path, _)),
    atom_codes(Change.text, TextCodes),
    assertz(doc_text(Path, TextCodes)).

%! doc_text_fallback(+Path:atom, -Text:text) is det.
%
%  Get the contents of the file at =Path=, either with the edits we've
%  been tracking in memory, or from the file on disc if no edits have
%  occured.
doc_text_fallback(Path, Text) :-
    doc_text(Path, Text), !.
doc_text_fallback(Path, Text) :-
    read_file_to_codes(Path, Text, []),
    assertz(doc_text(Path, Text)).

%! replace_codes_range(Text, StartLine, StartChar, EndLine, EndChar, ReplaceText, -NewText) is det.
replace_codes_range(Text, StartLine, StartChar, EndLine, EndChar, ReplaceText, NewText) :-
    phrase(replace_range(start, 0, 0, StartLine, StartChar, EndLine, EndChar, ReplaceText),
           Text,
           NewText).

replace_range(start, StartLine, StartChar, StartLine, StartChar, EndLine, EndChar, NewText), NewText -->
    !,
    replace_range(finish, StartLine, StartChar, StartLine, StartChar, EndLine, EndChar, NewText).
replace_range(finish, EndLine, EndChar, _, _, EndLine, EndChar, _) --> !, [].
replace_range(start, StartLine, 0, StartLine, StartChar, EndLine, EndChar, NewText), Take -->
    { length(Take, StartChar) },
    Take, !,
    replace_range(start, StartLine, StartChar, StartLine, StartChar, EndLine, EndChar, NewText).
replace_range(finish, EndLine, Char, StartLine, StartChar, EndLine, EndChar, NewText) -->
    !, { ToSkip is EndChar - Char },
    skip(ToSkip),
    replace_range(finish, EndLine, EndChar, StartLine, StartChar, EndLine, EndChar, NewText).
replace_range(start, Line0, _, StartLine, StartChar, EndLine, EndChar, NewText), Line -->
    line(Line),
    { succ(Line0, Line1) },
    replace_range(start, Line1, 0, StartLine, StartChar, EndLine, EndChar, NewText).
replace_range(finish, Line0, _, StartLine, StartChar, EndLine, EndChar, NewText) -->
    line(_),
    { succ(Line0, Line1) },
    replace_range(finish, Line1, 0, StartLine, StartChar, EndLine, EndChar, NewText).

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
