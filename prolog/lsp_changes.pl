:- module(lsp_changes, [handle_doc_changes/2,
                        doc_text/2]).

:- use_module(library(list_util), [drop/3,
                                   split_at/4,
                                   span/4]).

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
    skip_to_start(Text, StartLine, StartChar, PrefixText, RestText),
    drop(ReplaceLen, RestText, KeepText),
    append(ReplaceText, KeepText, Tail),
    append(PrefixText, Tail, NewText), !.

skip_to_start(Text, StartLine, StartChar, Prefix, Suffix) :-
    ( numlist(1, StartLine, NLines) ; NLines = [] ),
    foldl([_, Prefix0-Suffix0, Prefix1-Suffix1]>>(
              span(\==(0'\n), Suffix0, PrefixTail, [_|Suffix1]),
              append(Prefix0, PrefixTail, Prefix00),
              append(Prefix00, [0'\n], Prefix1)
          ),
          NLines,
          []-Text,
          PrefixLine-SuffixLine),
    split_at(StartChar, SuffixLine, LineHead, Suffix),
    append(PrefixLine, LineHead, Prefix).
