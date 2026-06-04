% Stackage ![](https://www.stackage.org/static/img/stackage.png){width=72 style="vertical-align: middle;"}\
  <small>_A community distribution of\
  Haskell source packages_</small>
% Jens Petersen `@juhp` (Red Hat)\
  and the Stackage Team
% _Haskell Ecosystem Workshop 2026_\
  \
  \
  <small><https://petersen.fedorapeople.org/talks/hew-2026-stackage.html>\
  <https://petersen.fedorapeople.org/talks/hew-2026-stackage-single.html></small>

[comment]: and the Haskell Ecosystem

# What is Stackage?
![](https://www.stackage.org/static/img/stackage.png){width=54 style="vertical-align: middle;"} [stackage.org](https://stackage.org)

## What is Stackage?
At its simplest, [Stackage](https://www.stackage.org) is a Haskell package source distribution

- Stackage publishes "snapshots"
- Each snapshot is a (small) subset of Hackage packages which can all be built together consistently
- There can only be one version of each included package in a given snapshot
- We also try to build testsuite and benchmarks as possible (and run testsuite as far as possible) ◻

# Stackage Team
Current curator team:

- Adam Bergmark (@bergmark) 2015
- Dan Burton (@danburton) 2015
- Jens Petersen (@juhp) 2015
- Mihai Maruseac (@mihaimaruseac) 2017
- Alexey Zabelin (@alexeyzab) 2018
- Joe Kachmar (@jkachmar) 2018
- Andreas Ländle (@alaendle) 2023
- Honorary member: Bryan Richter

<small>Grateful to Michael Snoyman (2014-2023)\
and remember Chris Dornan (2017-2024)</small>

## Stackage Team
Internal communication: this year we moved our private chat channel to Matrix.

Community project with many [Contributions](https://github.com/commercialhaskell/stackage/graphs/contributors?from=01%2F10%2F2012&to=31%2F05%2F2026)

# Stackage streams

currently: `nightly` and `lts`

# Stackage LTS stability

Properties and Invariants

- LTS stream has major versions
- Stackage LTS major versions preserve PVP X.Y versioning
  - ie no major version bumps are allowed to happen, except in special circumstances
- conversely `nightly` is a rolling release ◻

# Stackage size

| stream  | packages | ghc |
| --      | --   | -- |
|`nightly`| 3550 | 9.12.4 |
|`lts-24` | 3427 | 9.10.3 |
|`lts-23` | 3216 | 9.8.4 |
|`lts-22` | 3377 | 9.6.7 |

as of 3rd June

# Stackage LTS and ghc versions
- Current LTS 24 is based on ghc-9.10
- GHC minor version bumps are done within a LTS major version
  - LTS 23 was based on 9.8, etc ◻

## Stackage LTS and nightly
- LTS 25 will be branched from current `nightly` (ghc-9.12)
- Then `nightly` will move to ghc-9.14 &
  most stackage upperbound constraints will dropped ◻

# "A Very Brief History" of Stack and Stackage

- 2014-2015: Stackage and stack started by Michael Snoyman and FPComplete (FP Block)
  - Dec 2014: lts-0 released
  - 2015: initial release of stack

- 2015: Stackage curator team formed

- : (ffwd) ◻

## "A Very Brief History" of Stack and Stackage

- 2019: curator-2 switches to stack for building snapshot packages

- 2020: Stackage project joins Haskell Foundation

- 2022: Mike Pilgrem took over as maintainer of Stack

- 2023: Michael formally left the curator team

- 2024: Stackage server moves to HF and hosted within haskell.org ◻

##

_Stackage is a service provided by\
the Haskell Foundation_

# Commercial Haskell org on Github

These days the repos are all under:\
<https://github.com/commercialhaskell/>

- originally many were under `@fpco`. ◻

# Stackage Server

<https://stackage.org>

running on the same host as our build server.

<small><https://github.com/commercialhaskell/stackage-server>\
(yesod application)
</small>

## Tour of Stackage Server

<https://stackage.org>

# Stackage and stack

- Usage of Stackage usually means using the `stack` build tool
- Today I will only talk briefly about `stack`, but of course it is largely implied for general usage ◻

# Using Stackage snapshots

```
$ cat stack.yaml
```

```yaml
resolver: lts-24.43

# optionally can add or override deps by adding:
extra-deps:
  - my-fav-lib-0.2.1
  - another-dep-0.1
```

- (most package tarballs don't include stack.yaml) ◻

## Using Stackage snapshots
Alternatively from the cli:
```bash
$ stack --resolver lts-24 my-fav-lib-0.2.1 another-dep-0.1 .
```

<small>(note `resolver` is now deprecated in favour of `snapshot` ;)</small>

Plug for my `stack-all` utility

# Contributing

Most direct community communication happens through github:

- <https://github.com/commercialhaskell/stackage>
- <https://github.com/commercialhaskell/lts-haskell>

There are informal related Matrix channels:

- "Stack and Stackage":\
  `#haskell-stack:matrix.org`
- "Haskell distro packaging":\
  `#haskell-distros:matrix.org`

<small>_(rant: not really a fan of these combined channels like "Cabal and Hackage")_</small>

## Adding your package to Stackage

<https://github.com/commercialhaskell/stackage/#add-your-package>

## Demo

Let's add my package [`fontwhich`](https://www.stackage.org/) to nightly!

## Adding your package to LTS

<small><https://github.com/commercialhaskell/lts-haskell/blob/master/build-constraints/lts-24-build-constraints.yaml></small>


# Downstreams and users

<small>Important to understand that Stackage and Stack are quite separate projects,\
though of course codependent</small>

- Some Linux distributions follow Stackage LTS (not using stack):
  - Fedora: tracks `lts` major versions
  - Nixpkgs: tracks `lts` minor snapshots

- One _can_ use stackage snapshots with `cabal-install` `cabal.project`s with a little effort ◻

# Upstream interaction

- interactions with upstream projects often happens when new major or minor versions appear
- we open stackage issues for cross-package issues (typically bounds)
- we open upstream issues for testsuite and other problems

- recently we are helping to test ghc minor release RC's ◻

## Tracking of issues

Currently we have 55 [open issues](https://github.com/commercialhaskell/stackage/issues) for Stackage Nightly

- a bunch of them are preparation for ghc-9.14 ◻

# Building of Stackage

- stackage is built with a tool called `curator`, run by `build.sh`
  - <https://github.com/commercialhaskell/curator>
- actual package building is done by `stack` (since curator v2) ◻

## Building of Stackage

- building runs in an interactive shell/tmux on the buildserver
- use a container image for the build environment
- semi-automatic process, but needs human to fix problems interactively:
  - bounds issues
  - build failures, etc
- _haddocks and hoogle data is published_ (added after talk)
- Hackage distro data is published for `Stackage` and `LTSHaskell`
  - <https://hackage.haskell.org/distro/Stackage/packages> ◻

# Additional Tooling

- CI [./check](https://github.com/commercialhaskell/stackage/blob/master/check) on bounds (needs `curator`)
  - also `check-lts` nowadays
- [commenter](https://github.com/bergmark/commenter) mass bounds tool by Adam Bergmark (rust) ◻

# FAQ: GHC gap

Currently `nightly` is still on 9.12, while 9.14.1 already exists
 - and 10.0+ is on the horizon

# [Stackage](https://github.com/commercialhaskell/stackage/) `nightly` stats

- 3190 closed issues
- 4410 merged PRs (out of 4741)
- 21,234 commits
- 550 stars ◻

# Uniqueness of Stackage

Very few language ecosystems have an upstream source distribution like Stackage

## Plea to join and enlist!

Please base/add your projects on/to current Stackage snapshots!

- the best way to enter `lts` is via nightly
- `lts` is highly stable for deps, but even the demands of nightly are relatively low these days
  - except after branching when we prune most upperbounds

- Would be great if most major "official" projects targeted Stackage ◻

# Problems
- stack build churn for stackage snapshot building
- `lts` major version delays due to ghc minor release regressions
- need more help with improving tooling
  - curator
  - commenter
  - stackage-server
    - buildsystem visibility
  - user cli? ◻

## Problems

Conservative upperbounds

cause a lot of "busy work" in the ecosystem

- Personally I encourage people to use looser upperbounds when possible ◻

# Future
- `lts-25` should arrive once ghc-9.12.5 is released
- more automation
- try porting `commenter` to Haskell ;-)
- count users?
- add new curators to the team ◻

# Thank you

Happy building with stack and stackage!

Please talk to us about problems and ideas

<https://www.stackage.org>

![](https://www.stackage.org/static/img/stackage.png){width=90 style="vertical-align: middle;"}

<small><https://petersen.fedorapeople.org/talks/hew-2026-stackage.html>
