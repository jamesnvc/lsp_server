:- module(lsp_html_markdown, [ html_markdown/1 ]).
/** <module> LSP HTML->Markdown parser

Module to convert HTML into Markdown (by doing some transformations &
passing it on to lynx/html_text).

@author James Cash
*/

:- use_module(library(lynx/html_text), [ html_text/1 ]).

:- include('_lsp_path_add.pl').

html_markdown(DOM0) :-
    phrase(html_markdownify(DOM1), DOM0),
    html_text(DOM1).

html_markdownify([]) --> [].
html_markdownify([Elm1|Rest]) -->
    [ element(El, Attrs, Children) ], !,
    { markdownify_element(element(El, Attrs, Children), Elm1) },
    html_markdownify(Rest).
html_markdownify([Txt|Rest]) -->
    [Txt], { atom(Txt) }, !,
    html_markdownify(Rest).

markdownify_element(element(strong, Attrs, Children), Element) =>
    phrase(html_markdownify(Children0), Children),
    append(['*'|Children0], ['*'], Children1),
    Element = element(span, Attrs, Children1).
markdownify_element(element(em, Attrs, Children), Element) =>
    phrase(html_markdownify(Children0), Children),
    append(['**'|Children0], ['**'], Children1),
    Element = element(span, Attrs, Children1).
markdownify_element(element(code, Attrs, Children), Element) =>
    phrase(html_markdownify(Children0), Children),
    append(['`'|Children0], ['`'], Children1),
    Element = element(span, Attrs, Children1).
markdownify_element(element(var, Attrs, Children), Element) =>
    phrase(html_markdownify(Children0), Children),
    append(['`'|Children0], ['`'], Children1),
    Element = element(span, Attrs, Children1).
markdownify_element(element(pre, Attrs, Children), Element) =>
    Element = element(div, [],
                      ['```prolog\n',
                       element(pre, Attrs, Children),
                       '\n```']).
markdownify_element(element(El, Attrs, Children), Element) =>
    phrase(html_markdownify(Children0), Children),
    Element = element(El, Attrs, Children0).
