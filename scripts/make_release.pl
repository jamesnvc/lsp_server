:- module(make_release, []).

:- use_module(library(readutil), [read_file_to_terms/3]).

:- initialization(main, main).

increment_version(major, [Major0, _Minor, _Patch], [Major1, 0, 0]) :- !,
    succ(Major0, Major1).
increment_version(minor, [Major, Minor0, _Patch], [Major, Minor1, 0]) :- !,
    succ(Minor0, Minor1).
increment_version(patch, [Major, Minor, Patch0], [Major, Minor, Patch1]) :- !,
    succ(Patch0, Patch1).

update_pack_version(ReleaseType, NewVersion) :-
    read_file_to_terms('pack.pl', PackTerms, []),
    memberchk(version(OldVersion), PackTerms),
    atomic_list_concat([MajorS, MinorS, PatchS], '.', OldVersion),
    maplist(atom_number, [MajorS, MinorS, PatchS], VersionNums0),
    increment_version(ReleaseType, VersionNums0, VersionNums1),
    atomic_list_concat(VersionNums1, '.', NewVersion),
    once(select(version(OldVersion), PackTerms, version(NewVersion), NewPackTerms)),
    setup_call_cleanup(open('pack.pl', write, S, []),
                       forall(member(T, NewPackTerms),
                              write_term(S, T, [fullstop(true),
                                                nl(true),
                                                quoted(true),
                                                spacing(next_argument)
                                               ])),
                       close(S)).

git_commit_and_tag(NewVersion) :-
    shell('git add pack.pl'),
    shell('git commit -m "Bump version"'),
    format(atom(TagCmd), "git tag v~w", [NewVersion]),
    shell(TagCmd),
    format(atom(PushCmd), 'git push origin master v~w', [NewVersion]),
    shell(PushCmd).

register_new_pack(NewVersion) :-
    ( pack_remove(lsp_server) -> true ; true ),
    format(atom(Url),
           "https://github.com/jamesnvc/lsp_server/archive/refs/tags/v~w.zip",
           [NewVersion]),
    pack_install(lsp_server, [url(Url), interactive(false)]).

main([ReleaseType]) :-
    update_pack_version(ReleaseType, NewVersion),
    format("Bumping to ~w~n", [NewVersion]),
    git_commit_and_tag(NewVersion),
    register_new_pack(NewVersion).
