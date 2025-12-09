#!/usr/bin/env swipl
:- module(make_release, []).

:- use_module(library(readutil), [read_file_to_terms/3,
                                  read_line_to_string/2]).

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

download_pattern_format_string(DownloadURLPat, FormatString) :-
    string_concat("https://github.com", _, DownloadURLPat), !,
    % Github download locations are special-cased
    string_concat(Prefix, "releases/*.zip", DownloadURLPat),
    string_concat(Prefix, "archive/refs/tags/v~w.zip", FormatString).
download_pattern_format_string(DownloadURLPat, FormatString) :-
    file_name_extension(Base0, Ext, DownloadURLPat),
    string_concat(Base, "*", Base0),
    format(string(FormatString), "~s~s.~s", [Base, "v~w", Ext]).

register_new_pack(NewVersion) :-
    read_file_to_terms('pack.pl', PackTerms, []),
    memberchk(name(ProjectName), PackTerms),
    memberchk(download(DownloadURLPattern), PackTerms),
    download_pattern_format_string(DownloadURLPattern, URLFormat),
    ( pack_remove(ProjectName) -> true ; true ),
    format(atom(Url), URLFormat, [NewVersion]),
    pack_install(ProjectName, [url(Url), interactive(false)]).

main(Args) :-
    ( Args = [ReleaseType], increment_version(ReleaseType, [0, 0, 0], _)
    -> true
    ;  ( format(user_error, "Usage: make_release.pl [major|minor|patch]~n", []),
         halt(1) ) ),
    ( stream_property(user_input, tty(true))
    -> format("Make new release? [y/n]: ", []),
       read_line_to_string(user_input, Input),
       Input == "y"
    ; true ),
    update_pack_version(ReleaseType, NewVersion),
    format("Bumping to ~w~n", [NewVersion]),
    git_commit_and_tag(NewVersion),
    register_new_pack(NewVersion).
