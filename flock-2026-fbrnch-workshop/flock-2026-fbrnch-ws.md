---
title: |
  `fbrnch` packaging\
  workshop
  ![](https://www.fedoraproject.org/assets/images/logos/fedora-logo.svg){width=256 class="title-logo"}
author: "Jens Petersen `@juhp` (Red Hat)"
date: |
  _Flock 2026, Prague_

  <https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws.html>\
  <small><https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws-single.html></small>

  `sudo dnf [install|update] fbrnch`
header-includes:
  - |
    <style>
    div.section-divider {
        text-align: center !important;
        padding-top: 35vh !important; /* % of viewport height */
    }
    div.section-divider h1 {
        font-size: 3em !important;
    }
    #title-slide {
        position: relative;
    }
    .title-logo {
        position: absolute;
        top: 40px;
        right: 40px;
    }
    </style>
---

# `fbrnch` Workshop overview (vision)

- opening and introduction
- demos
- deeper dive
- hands-on time?
- more examples
- Q&A and feedback discussion

# Installation

Please install `fbrnch`:

Latest stable version is 1.8.3

`sudo dnf [install|update] fbrnch`

# Opening {.section-divider}

What do you want to get out of this workshop?

# Slides

<https://tinyurl.com/flock26-fbrnch> 👉
![](flock26-fbrnch-qr.png){width=256 class="title-logo"}

<https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws.html>

<small>

- [https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws<u>-single</u>.html](https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws-single.html)
- [https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws<u>-embed</u>.html](https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws-embed.html)

</small>

Source: <https://codeberg.org/juhp/fbrnch-workshop/>

# Introduction {.section-divider}

# Workshop Objectives

AMA/BoF -> your ideas/input

- teaching: to increase understanding of `fbrnch`
- adoption: to get more people using `fbrnch`
- planning: future priorities

What can `fbrnch` do for you?

## My questions
What has stopped you from using `fbrnch`?

What would encourage you to adopt it?

# What is fbrnch?

`fbrnch` is a helpful tool for Fedora packagers

- particularly those who maintain larger sets of packages

- goal is to improve the daily life of and lower the burden of packagers

- `fbrnch` tries to be <b>idempotent</b> where it can

# Overview of `fbrnch`

kind of "`fedpkg` _on steroids_", but much more

## merging and building a package across release branches
- downloads new tarballs
- e.g. catches missing sources
- native watchtask
- offers to merge branches
- `bump`
- builds can be "resumed" (watchtask)

## automated parallel builds of sets of packages in dependency order
- `sort` with chain-build output

## local builds with deps installation
- local installation
- also mock wrapper

## easy scratch builds
- `scratch-x86_64`

## progressive copr builds
- by release, arch, etc

## creating, updating and listing package reviews

## requesting new repos and branches

## importing new packages
- request repos
- request branches

with existence checks

## pagure queries
(like `pagure-cli`)
- `owner`
- `list`

## bz queries
- `bugs`
- `bzusers`
- `find-review`
- `reviews`
- `ftbfs`

## other helpers
- rename master branches to rawhide
- autospec and unautospec
- compare tarballs
- sidetags
- waitrepo and watchtask
- package repoquery
- `command`: run arbitrary commands over package dirs

# Some facts

5 years old

- first official release: 0.7.1 (2021-02-09)
- Fedora F34 Change: <https://fedoraproject.org/wiki/Changes/fbrnch> (2021)

- current version: 1.8.3 (released 2026-06-03)

- repo: <https://github.com/juhp/fbrnch>
- currently: 1940 commits

- releases: <https://hackage.haskell.org/package/fbrnch>
- package: <https://src.fedoraproject.org/rpms/fbrnch>


## Major releases
<https://github.com/juhp/fbrnch/tags>

- 0.7: Feb 2021
- 0.8: Apr 2021
- 0.9: Jul 2021
- 1.0: Feb 2022
- 1.1: Apr 2022
- 1.2: Nov 2022
- 1.3: Apr 2023
- 1.4: May 2024
- 1.5: Aug 2024
- 1.6: Dec 2024
- 1.7: Jun 2025
- 1.8: Jan 2026

# History

- `fbrnch` started as a very simple tool to build across fedora branches
- then gradually grew to include package review workflows and later copr, etc
- I wanted support well the lifecycle of a package, e.g.:
  - starting from packages review workflow
  - importing an approved package
  - building it across branches
  - updating the package
  - handling bugs and failures

# Demos {.section-divider}

## Let's build a package!

# Update a branch

Let's build latest `racket` for epel-10.3

<https://bugzilla.redhat.com/show_bug.cgi?id=2482488>

<https://src.fedoraproject.org/rpms/racket>

```
$ fbrnch build epel10 -f f44
```

# Update rawhide package version

## git-annex

<https://bugzilla.redhat.com/show_bug.cgi?id=2407096>

```
$ fbrnch build
```

::: {.incremental}
- `update-sources`
:::

# Multipackage updates

## cmark & dependents

Updating cmark from 0.31.1 to 0.31.2 (strict sonames)

<https://bugzilla.redhat.com/show_bug.cgi?id=2439840>

- cf [previous](https://bugzilla.redhat.com/show_bug.cgi?id=2260869)
- [update](https://bodhi.fedoraproject.org/updates/FEDORA-2026-2249864f96)

(`frpq` = `fedora-repoquery` - a symlink in `fedora-repoquery-short`)

```
$ frpq 45 --wr 'libcmark.so.0.31.1()(64bit)' --qf "%{source_name}"
cmark
evolution
foundry
gnome-builder
mkvtoolnix
neochat
nheko
perl-Clownfish-CFC
perl-CommonMark

$ echo $(frpq 45 --wr 'libcmark.so.0.31.1()(64bit)' --qf "%{source_name})"
cmark evolution foundry gnome-builder mkvtoolnix neochat nheko perl-Clownfish-CFC perl-CommonMark
$ fbrnch switch rawhide $(frpq 45 --wr 'libcmark.so.0.31.1()(64bit)' --qf "%{source_name}")
$ fbrnch pull $(frpq 45 --wr 'libcmark.so.0.31.1()(64bit)' --qf "%{source_name}")
$ fbrnch sort $(frpq 45 --wr 'libcmark.so.0.31.1()(64bit)' --qf "%{source_name}")
cmark : evolution foundry gnome-builder mkvtoolnix neochat nheko perl-Clownfish-CFC perl-CommonMark
$ fbrnch bump -m "F45 rebuild against cmark" rawhide ...
$ fbrnch parallel rawhide --sidetag ...
```

- `parallel` builds in a sidetag
- offers to submit sidetag to bodhi
-

# Copr building

Update lean4 copr:

```
$ fbrnch copr lean4
```

(Actually also newer `elan` is also available;)

# Dependency Graph

```
$ fbrnch graph *
```

# Your favorite examples here!

# List of all subcommands

```
$ fbrnch --list-commands
autospec
branch-log
branches
bugs
build
builddeps
bump
bzusers
clone
command
commit
compare-branches
compare-tarballs
copr
count
create-review
diff
fetch
find-review
ftbfs
graph
import
install
list
list-local
local
merge
mock
move-artifacts
not-installed
nvr
override
owner
parallel
prep
pull
push
rename-rawhide
repoquery
request-branches
request-repos
review-package
reviews
scratch
scratch-aarch64
scratch-x86_64
sidetags
sort
src-deps
srpm
srpm-spec
status
switch
tag-build-to-sidetag
unautospec
uninstall
unpushed
update-review
update-sources
waitrepo
watchtask
```

# 1.8 changes  {.section-divider}

<https://hackage.haskell.org/package/fbrnch-1.8.3/changelog>

# 1.8.3 (2026-06-03)
- `compare-tarballs`: order tarballs by removal and addition

# 1.8.2 (2026-06-03)
- `autospec`: don't switch to rawhide and pass pkg spec file to convert
- `copr`: filter out canceled builds from existing builds
- `mock`: add `--no-pull` and `--config-options`
- `mock`: use target branch for srpm and build disttag
- `nvr`: fix for epel10 disttag with autorelease
- Branches listOfBranches: do eol/branched checks for all BranchesReq

# 1.8.1 (2026-03-20)
- new native watchtask implementation and command
- local build.log's now generated with 'script' tool instead of tee pipe

- `compare-tarballs`: include extension in file args help
- `copr`: retry coprGetBuild when watching a build
- `install`: take %_rpmdir into account
- `local`,`install`: rename `--rebuild` to `--force-build`
- `parallel`: for single package do full kojiWaitTask
- `parallel`: notify-send for failure or `--must-push` failure
- `parallel`: use reverse text to highlight problem prompts
- `pull`: use retry
- `request-branches`: include branches in comment
- `update-sources`: for "sources" diff also compare-tarballs
- `waitrepo`: rename `--no-nvr` to `--no-package`
- `watchtask`: displays build hosts initially
- `watchtask`: can watch ongoing package branch build; `--progress`
- `watchtask`: improve formatting with arch padding and list hosts host
- Koji getChildrenTasks: use maybeTimeout to avoid crash
- Koji: introduce kojiGetTaskState' for DRY
- Koji: kojiBuild output profile if uploading srpm
- Koji: retry with kojiGetTaskChildren seems to eliminate timeouts yay
- Revert "Package isArchiveFile: ".sig" files are ascii"
- RpmBuild checkSourcesMatch: improve logic for missing sources
- gitMergeOrigin: offer to rebase if remote was ahead
- kojiWaitRepo: now implemented by kojiWaitRepoNVRs
- kojiWaitRepoNVRs: check krb ticket before calling koji
- kojiWatchBuildTask loopWatch: check parent state not number of buildArchs
- kojiWatchBuildTask: track display of NVR (in Cyan)

# 1.8 (2026-01-02)
- `branch-log`: new command to check local and remote branches
- `branch-log`: `--latest` finds connected branches
- `branch-log`: logic to warn about older branch being ahead
- `build --sidetag`: fix "release not found for branch f43" rawhide regression
- `build`: use `--ref` to build from a commit
- `build`,`parallel`: can now merge `--from` a particular branch
- `commit`: error if changelog is unstaged
- `commit`: require -b option to confirm non-rawhide commit
- `compare-branches`: renamed from `compare`
- `compare-branches`: list changes for both local & remote branches
- `compare-tarballs`: new command to diff source tarballs
- `compare-tarballs`: add `--max-line-length` option (default 200)
- `copr --failures`: fetches tails of failed build logs
- `copr --monitor`: add `--filter` to only list results matching needle
- `copr --monitor`: add `--name-only` to only print package name with filter
- `copr`: handle user/project (for queries, etc)
- `install`: select-rpms-0.3.1 allows selecting debuginfo subpkgs
- `install`: select-rpms-0.3.1 adds `--allow-erasing`
- `install`: options to select package manager to use (eg `--rpm`)
- `install`: use needToBuildRPMS with prompt, factored out of buildRPMs
- `repoquery`: use fedora-repoquery
- `review-package`: for licensecheck find build source dir & use SPDX shortnames
- `review-package`: ignore error for non-zero diff
- `review-package`: download upstream with spectool and prep if not building
- `review-package`: prompt if bugid review not in NEW/ASSIGNED state
- `scratch`: exclude i386 as i686
- `switch`: add `--stash`
- `update-sources`: do sanity prep in a tmpdir to avoid any ongoing build
- `update-sources`: offer to compare tarballs
- `update-sources --force`: need to partition all sources not previously existing
- `update-sources`: detect version change by diff better again
- `update-sources`: if Release was already bumped then use it
- `FBRNCH_KOJI_PROFILE` provides experimental Koji profile support (eg riscv)
- bodhiUpdate: switch back to branch if necessary
- mergeBranch: colorize the unmergeable warning
- mergeBranch: relax the unmergeable prompt condition for no unpushed commits
- mergeBranch: warn about unpushed commits in newer branch
- mergeBranch: setup local branch with tracking if missing
- use rpmbuild-order-0.4.13: handle cmake dirs and python3dist dyn provides
- Branches getRequestedBranches: error if no active branches requested
- Branches: new listBranches/listAllBranches & listRemoteBranches/listRemoteAllBranches
- Git gitOneLineLog: add '`--`' after range to avoid path confusion
- Git gitPush: make sure upstream branch is setup

# Documentation

WIP!

- trying to generate docs in a maintainable way (use mdsh)

For now: only `--help` output

# fbrnch development

<https://hackage.haskell.org/package/fbrnch-1.8.3/fbrnch.cabal>

<https://github.com/juhp/fbrnch#readme>

## Haskell libraries

- fedora-releases
- rpmbuild-order
- rpm-nvr
- select-rpms

- bodhi-hs
- bugzilla-redhat
- copr-api
- (fedora-krb)
- koji
- pagure

more planned: fedora-repoquery, koji-tool

# Plea to join and contribute! {.section-divider}

# Difficulties overcome

- PDC migration to Bodhi Releases (fedora-dists to fedora-releases)
- `waitrepo`
- scrapers, anubis, and network timeouts (http `retry`s)

# Future plans and ideas

- blog post and docs
- AI: Agents? MCP? Skills?
- Forgejo
- Packit? (no plans as yet)
- support user forks!
- `EDITOR` support
- git worktrees?

## Problems
<https://github.com/juhp/fbrnch/issues>

- todo's and open issues
- would like more feedback and reports
- I don't use fedpkg much
  - also as developer I tend not to run fbrnch releases daily
- did I say it was written in Haskell?
- time resource: so far no LLM agent usage
- only 25 github stars 😢

# Need your feedback {.section-divider}

- open an issue or report a bug

# Thank you

Happy building with `fbrnch`

Please report problems and contribute ideas!

<https://github.com/juhp/fbrnch>

<https://petersen.fedorapeople.org/talks/flock-2026-fbrnch-ws.html>

# Appendix: my other Tools

## Packaged in Fedora
- pagure-cli
- koji-tool
- dl-fedora
- dnf-repo
- fedora-repoquery

- lsfrom
- ostree-pin
- rhbzquery
- rpmbuild-order
- pkgtreediff
- hwk
- (bash-color-prompt)

## Additional tools
- df-simple
- fhcontainer
- bodhi-cli
