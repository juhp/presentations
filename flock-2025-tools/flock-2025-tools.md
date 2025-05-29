% fbrnch, koji-tool, dl-fedora, dnf-repo, and other tools
% Jens Petersen\
  `petersen@redhat.com`
% Flock 2025, Prague\
  \
  \
  <small><https://petersen.fedorapeople.org/flock-2025-tools.html></small>

# Overview

- pagure-cli
- fbrnch
- koji-tool
- dl-fedora
- dnf-repo

time allowing:

- lsfrom
- fedora-repoquery
- ostree-pin
- rhbzquery
- pkgtreediff

- (bash-color-prompt)

All packaged for Fedora and under <https://github.com/juhp>

Also all written in Haskell ;-)

# pagure-cli
<https://github.com/juhp/pagure-cli#readme>

Only cli tool in Fedora for querying pagure!!

Defaults to fedora dist-git instance:

```shellsession
⬢ fedora43~$ pagure -h
Pagure client

Usage: pagure [--version] COMMAND

  Simple pagure CLI

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list                     list projects
  user                     list user repos
  group                    list group repos
  project                  show project details
  branches                 list project branches
  git-url                  show project repo's git urls
  issues                   list project issues
  issue                    show project issue
  users                    list users
  username                 fullname of user
  userinfo                 show user details
  groups                   list groups
  groupinfo                show group details
```

Also handy for simple package stats:
```shellsession
⬢ fedora43~$ pagure list -c rust-*
3770
⬢ fedora43~$ pagure list -c python-*
3158
⬢ fedora43~$ pagure list -c rubygem-*
434
⬢ fedora43~$ pagure list -c ghc-*
636
⬢ fedora43~$ pagure list -c ocaml-*
172
⬢ fedora43~$ pagure list -c gtk*
44
⬢ fedora43~$ pagure list -c qt*
121
⬢ fedora43~$ pagure groups -c
68
$ pagure group -c rust-sig
3913
```

# fbrnch
<https://github.com/juhp/fbrnch#readme>

_Power packaging tool for consistent workflows_

Paricularly useful for packagers dealing with large package sets

Announcing fbrnch-1.7 released in Rawhide

# `fbrnch --help`

```shellsession
$ fbrnch -h
Fedora branch building tool

Usage: fbrnch [--version] COMMAND

  A tool to help with updating and building package branches
  https://github.com/juhp/fbrnch#readme

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  clone                    Clone packages
  switch                   Switch branch
  nvr                      Print name-version-release
  status                   Status package/branch status
  merge                    Merge from newer branch
  unpushed                 Show unpushed commits
  build                    Build package(s) in Koji
  list                     List packages in pagure distgit
  list-local               List packages in branch
  branches                 List package branches
  parallel                 Parallel build packages in Koji
  sidetags                 List user's side-tags
  override                 Tag builds into buildroot override in Koji
  waitrepo                 Wait for build to appear in Koji buildroot
  scratch                  Scratch build package in Koji
  scratch-aarch64          Koji aarch64 scratch build of package
  scratch-x86_64           Koji x86_64 scratch build of package
  update-sources           Download and update newer sources
  sort                     Sort packages in build dependency order (default
                           format: chain-build)
  prep                     Prep sources
  local                    Build locally
  srpm                     Build srpm
  srpm-spec                Show the spec file in an srpm
  diff                     Diff local changes
  compare                  Show commits between branches
  src-deps                 List source package dependencies
  mock                     Local mock build
  builddeps                Install package build dependencies
  install                  Build locally and install package(s)
  not-installed            Packages not installed locally
  uninstall                Remove installed package(s)
  bugs                     List package bugs
  bump                     Bump release for package
  commit                   Git commit packages
  pull                     Git pull packages
  fetch                    Git fetch packages
  push                     Git push packages
  owner                    List package owner(s)
  bzusers                  Search bugzilla users
  create-review            Create a Package Review request
  update-review            Update a Package Review
  review-package           Run fedora-review on a package Review Request bug
  reviews                  List package reviews
  request-repos            Request dist git repo for new approved packages
  import                   Import new approved created packages from bugzilla
                           review
  request-branches         Request branches for approved created packages
  find-review              Find package review bug
  command                  Run shell command in package dirs ($p)
  copr                     Build package(s) in Fedora Copr
  rename-rawhide           Rename local 'master' branch to 'rawhide'
  count                    Count number of living packages
  graph                    Output dependency graph
  ftbfs                    Check FTBFS status
  autospec                 Convert package to use rpmautospec
  unautospec               Unconvert rpmautospec package
  move-artifacts           Move old rpm artifacts into rpmbuild dirs
  tag-build-to-sidetag     Tag NVR for current branch to sidetag
```

# fbrnch 1.7 (2025-06-05)

_Let's build it for F42!_

- support for epel10.x minor branches
- `merge`s from origin
- `--sidetag` prompts to choose sidetag
- experimental `mock --install` option
- `parallel` offers to retry failures
- `request-branches` also checks access via group
- `request-branches` checks koji listing of packages after all requests
- `review-package` defaults to streamlined interactive mode
- new `uninstall`
- `update-sources` recognizes .crate's
- checks for close EOL of branch

# fbrnch 1.6.2 (2025-03-05)

- `local --detached-head`
- `request-branches`: now waits for package to be listed for branch buildtag
- `sidetags --tagged` lists tagged builds
- `unautospec`

# fbrnch 1.6 (2024 Dec)

- wait-repo now just uses "`koji wait-repo --request`"
- `install`: add `--existing-only`, `--skip-existing`, `--no-reinstall` options from select-rpms-0.2 (ported from koji-tool install)
- `request-repo`: now offers to import the new repo immediately
- `scratch-x86_64`,`scratch-aarch64`: `--exclude-arch` inversion no longer fast fails

## 1.5 (2024 Aug)

- fedora-releases (using Bodhi) replaces fedora-dists (PDC): explicit imports
- `copr`: no longer rebuilds nvr's unless `--force`
- `copr`: does not resubmit existing successful or in-progress nvr's
- `install`: use select-rpms library refactored from koji-tool
- `parallel`: support chain-build args with colon interspersed layers
- `prep`: `--allow-head`
- `sort`: default to chain-build output
- `update-sources`: git adds patches

# rpmbuild-order
<https://github.com/juhp/rpmbuild-order/>

Orders package source dirs in build dependency order.

- sort is still not perfect: problems with cmake deps
  - difficult/impossible to generate all deps before building

# Other related Libraries

## [fedora-releases](https://hackage.haskell.org/package/fedora-releases)
Just released 0.3.0 with epel-10.x support

used by:
- fbrnch
- dl-fedora
- fedora-repoquery

## [select-rpms](https://hackage.haskell.org/package/select-rpms)

used by fbrnch and koji-tool

Give usage examples

## [rpm-nvr](https://hackage.haskell.org/package/rpm-nvr)

used by: fbrnch koji-tool pkgtreediff select-rpms

# dnf-repo
<https://github.com/juhp/dnf-repo>

- control local yum .repo's with pattern matching

# `dnf-repo --help`

```
DNF wrapper repo tool

Usage: dnf-repo [--version] [-n|--dryrun] [-q|--quiet] [-D|--debug] [-l|--list]
                [-s|--save] [-4|--dnf4] [(-w|--weak-deps) | (-W|--no-weak-deps)]
                [--exact]
                [(-d|--disable REPOPAT) | (-e|--enable REPOPAT) |
                  (-o|--only REPOPAT) | (-x|--expire REPOPAT) |
                  (-X|--clear-expires) | (-E|--delete-repofile REPOPAT) |
                  (-z|--time REPOPAT) | (-t|--enable-testing) |
                  (-T|--disable-testing) | (-m|--enable-modular) |
                  (-M|--disable-modular) | --enable-debuginfo |
                  --disable-debuginfo | --enable-source | --disable-source |
                  (-c|--add-copr [SERVER/]COPR/PROJECT|URL) [--osname OSNAME]
                  [--releasever RELEASEVER] |
                  (-k|--add-koji REPO) | (-r|--add-repofile REPOFILEURL)
                  [--releasever RELEASEVER] |
                  (-u|--repourl URL)] [DNFARGS]

  see https://github.com/juhp/dnf-repo#readme

Available options:
  -h,--help                Show this help text
  --version                Show version
  -n,--dryrun              Dry run
  -q,--quiet               Suppress necessary output
  -D,--debug               Debug output
  -l,--list                List all repos
  -s,--save                Save the repo enable/disable state
  -4,--dnf4                Use dnf4 (if dnf5 available)
  -w,--weak-deps           Use weak dependencies
  -W,--no-weak-deps        Disable weak dependencies
  --exact                  Match repo names exactly
  -d,--disable REPOPAT     Disable repos
  -e,--enable REPOPAT      Enable repos
  -o,--only REPOPAT        Only use matching repos
  -x,--expire REPOPAT      Expire repo cache
  -X,--clear-expires       Undo cache expirations
  -E,--delete-repofile REPOPAT
                           Remove unwanted .repo file
  -z,--time REPOPAT        Show repodata timestamps
  -t,--enable-testing      Enable testing repos
  -T,--disable-testing     Disable testing repos
  -m,--enable-modular      Enable modular repos
  -M,--disable-modular     Disable modular repos
  --enable-debuginfo       Enable debuginfo repos
  --disable-debuginfo      Disable debuginfo repos
  --enable-source          Enable source repos
  --disable-source         Disable source repos
  -c,--add-copr [SERVER/]COPR/PROJECT|URL
                           Install copr repo file (defaults to fedora server)
  --osname OSNAME          Specify OS Name to override (eg epel)
  --releasever RELEASEVER  Specify OS Release Version to override (eg rawhide)
  -k,--add-koji REPO       Create repo file for a Fedora koji repo (f40-build,
                           rawhide, epel9-build, etc)
  -r,--add-repofile REPOFILEURL
                           Install repo file
  --releasever RELEASEVER  Specify OS Release Version to override (eg rawhide)
  -u,--repourl URL         Use temporary repo from a baseurl
```

# koji-tool
<https://github.com/juhp/koji-tool>

- commands for querying koji `builds` and `tasks`
- `progress` and `install`
- also friendly `find` command

- plan to convert to a library, so fbrnch can use it directly

## recent koji-tool features

### 1.3 (2025-05-03)
- 'install': add --tagged to allow installing an entire (side)tag
- 'tasks': add --rootlog (and "rootlog" for find)

# dl-fedora
Download Fedora iso images

## recent dl-fedora features

### 2.0 (2025-03-07)
- partial downloads are now staged in a `.dl-fedora-partial/` subdirectory
- promote KDE to edition
- add `--all-desktops`
- add `--dir` option to override download dir

### 1.3 (2025-02-16)
- F42 Workstation now created with Kiwi
- KDE spin renamed to KDE Desktop
- add COSMIC for F42
- use "c{9,10}s-live" for alt live respins instead of --cs-live-respin
- support downloading multiple or all editions/spins:
  --all-editions and --all-spins are defined per release or respin
- handle Kiwi use for different versions
- rawhide can now be specified by version number (uses fedora-release)
- add release aliases 'current' and 'previous'

### 1.2.1 (2024-11-01)
- add MiracleWM and KDE_Mobile Live images

# Extra content

# fedora-repoquery
<https://github.com/juhp/fedora-repoquery>

- query and cache repodata for different releases

Build new release

## Recent features

### 0.7.3 (2025-06-03)
- add --queryformat "n" abbreviation
- support riscv64 repoqueries in fedora.riscv.rocks

### 0.7.2 (2025-05-08)
- warn when no release specified
- add support for epel10.x minor releases
- also parse cNs (c8s, c9s, c10s)
- use eln via download.fp.o (or dl.fp.o) and drop its channels
- support short pneumonics for common queryformats (nv,nvr,nvra,envr,envra)
- improve branched pre-release logic using fedora-releases for Bodhi API

### 0.7.1 (2024-09-08)
- enable updates-testing for fedora branched from Beta freeze
- help: can use -- for system repoquery

## Plans
- want to make it into a library so other tools can use it directly

# lsfrom

# ostree-pin

# rhbzquery

# fedora-composes
<https://copr.fedorainfracloud.org/coprs/petersen/fedora-composes>

# Extra slide

## Bonus slide: bash-color-prompt
0.6 is now in rawhide!

Lennart requested enabling for more terminals

## pkgtreediff

## hwk

## fhcontainer
<https://copr.fedorainfracloud.org/coprs/petersen/fhcontainer/>

## df-simple
<https://github.com/juhp/df-simple>
