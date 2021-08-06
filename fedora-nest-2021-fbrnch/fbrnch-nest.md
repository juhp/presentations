---
title: fbrnch @ Nest with Fedora
author: Jens Petersen (@juhp) - 2021
patat:
    wrap: true
    margins:
        left: 10
        right: 10
...

```
  __ _                     _
 / _| |__  _ __ _ __   ___| |__     One Year On...
| |_| '_ \| '__| '_ \ / __| '_ \
|  _| |_) | |  | | | | (__| | | |   or how I can push a stack of
|_| |_.__/|_|  |_| |_|\___|_| |_|   600 packages to Koji in 3 days
```

fbrnch is a commandline tool to help Fedora Packagers

It builds fedora packages across branches
    - repetitive work which can be largely automated

# FBrnch (fedora branch)

fbrnch: Fedora branch packaging tool

- handles both ***dist-git*** and ***non-distgit***:
    - koji scratch builds
    - can replace daily rpmbuild, koji, fedpkg, bodhi
    - prep/local/srpm/install/mock packages locally
- build packages
    - commit, bump, pull
    - merging branches & pushing updates to Bodhi
    - automated __parallel__ koji builds in dependency layers
- list, create and update _package reviews_ from spec files
    - repo request and import directly from bugzilla review
- diff local changes (when changing many packages)
- show package status and bugs
- install locally built packages
- push incremental Copr builds
- run custom command

# Install fbrnch!

I recommend you install `fbrnch-0.9.1.1` and play around with it:

```
$ sudo dnf --enablerepo=updates-testing install fbrnch
$ fbrnch --help
```

# Development since 7 August 2020

- 485 commits
- 14 github issues closed
- 13 releases
- https://hackage.haskell.org/package/fbrnch since Feb 2021
- In Fedora package since 20 Feb 2021
- https://fedoraproject.org/wiki/Changes/fbrnch F34 Change
- 27 Bodhi package updates

# 0.9.x Highlights

0.9.2 *(next version in development)*

- changed `%_sourcedir` handling
- `diff --status` and `--filter`
- `parallel`: output improvements
- catch detached working dir (eg rebase)
- `bump --local`
- new `count` command
- (planned) repoquery

0.9.1.1 *(currently in Fedora `updates-testing`)*

- bugfix
- `reviews`: add --pattern for package prefix

0.9.1

- `local` now writes .build.log
- `reviews`: pre-sort by bug id
- `scratch`: --ref to specify a commit other than HEAD to build
- `command` enhancements
- check for patches as well as sources

0.9 *(current version in Fedora `updates`)*

- new `waitrepo` command and `override --duration`
- `build` `--no-merge` and `--merge`
- `copr`: native watch-build
- `branches`: add `--current` and support `--missing`
- `parallel`: native koji waitTask
- `sidetags`: default to all user's sidetags
- new `graph` command renders dependency graph using graphviz
- `srpm` regeneration now checks sources timestamp

# 0.8.0 Highlights

- major refactor of branch-package args handling together with branch options

- `request-repos` can take branch args and `--mock` option (#18)
- extend dryrun to bodhiCreateOverride, putBugBuild, bodhiUpdate, kojiWaitRepo
- drop the restriction of no packages inside a pkg dir (#19)
- `parallel`: use parallelBranches for single package arg
- `request-branch`: allow request with closed pkg review
- `copr`: new --list-chroots option for project

# 0.7.x Highlights

0.7.3

- `scratch`: don't get sources for pushed git build
- Build: refine the "still in testing" logic to check testing repo with prompt
- Koji: offer to resubmit build on error
- `request-repos` now prompts for branching
- add 'not-installed' command: lists packages not installed locally at all
- `srpm`: add `--force` option
- `rename-master`: renamed from 'master-rename'
- `local --force`: ignore existing built rpms

0.7.2

- 'install': --recurse to install missing neighboring deps
- 'local','install': print package name when build fails
- Bodhi overrides: error if failed; use 4 days

0.7.1

- first Hackage release

0.7.0

- reworked branch/pkg arg processing
  which allows `branches --remote` to work without a repo
- `local`/`install`: `--with`/`--without` bcond options
- `build`: offer to create a Bodhi update for an already built candidate nvr
- new `update` command: experimental package version updating
- 'import' prompts for branching after build
- 'parallel': only use --background if >5 packages in layer (#17)
- new 'master-rename' command: renames package master branches to rawhide

# 0.6.x Highlight

0.6.8 (2021-01-16)

- build: check bodhi client new update success more carefully
- Bugzilla: correctly check that bug update succeeded
- new 'list' command to list packages from pagure

0.6.7 (2020-12-23)

- build/install: allow no branch arg for current directory
- request-repos: prompt for reviewer thanks
- request-repos: added --all-states for Modified

0.6.5 (2020-12-03)

- support git worktrees (experimental)
- git fetching now outputs new branches
- build: only wait-repo if overriding or autoupdate
- mock and scratch: --dryrun
- add --all-fedora and --all-epel branch options (#15)

0.6.4 (2020-11-12)

- import: offer to request-branches after build
- request-repo: thank reviewer by first name

0.6.3 (2020-10-21)

- new 'branches' command lists package's branches

0.6.1 (2020-10-17)

- build, parallel: request testing state for bodhi updates
- bugs: --summary to filter by a phrase
- reviews: --user and --assigned-to

# Demos

# Handling of ~/rpmbuild/

rpm defaults to using ~/rpmbuild/ for SOURCES, SPECS, BUILD, (S)RPMS

fedpkg requires pkggit and ignores ~/rpmbuild/

0.9.2 to use ~/rpmbuild/SOURCES as a cache

# Technical debt

- Fedora infra service libraries still bundled vendored
- no native authentication
- check git hashes of existing builds
- use Path type

# What's next

- 'repoquery' command (fedora-repoquery)
- locally git tag koji builds
- 'worktree'?

- rpmautospec

- koji-install, koji-progress, koji-query

- Centos Stream support

# More info

Source: <https://github.com/juhp/fbrnch>

- TODOs and FIXMEs

You can try fbrnch today: `dnf install fbrnch`

CI: <https://github.com/juhp/fbrnch/actions>

You may also like: dl-fedora, lsfrom, pkgtreediff, rhbzquery

Thanks to the contributors so far!
