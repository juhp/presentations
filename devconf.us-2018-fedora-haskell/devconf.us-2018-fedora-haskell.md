% ![](20110717032101!Fedora_infinity.png "Fedora Logo"){ width=128 } ![](HaskellLogoStyPreview-1.png "Haskell Logo")\
  Fedora Haskell Packaging and Building
% Jens Petersen
% Devconf.us 2018, Boston (Aug 18)

## I am Jens Petersen

Red Hat and Fedora

Fedora Haskell project and SIG founder

<https://petersen.fedorapeople.org/talks/devconf.us-2018-fedora-haskell>

# Haskell

## Why Haskell and GHC?

- Functional purity and immutability
- Statically and strongly typed
- Good concurrency
- Lazy evaluation
- Native executables
- Garbage collected

<small><https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique></small>

## Famous Haskell projects?

## Haskell projects

- Pandoc
- ShellCheck
- Xmonad and xmobar
- git-annex
- hledger
- Hakyll
- Shake
- Weldr

## Haskell also great for writing programming languages

eg: ghc, Purescript, Elm, Agda, idris, ...

## [Enterprise Haskell](http://industry.haskell.org/)

- Facebook
- Banks (financial trading, etc)
- Cardano blockchain
- etc

Consulting: Well Typed, FP Complete, ...

# Haskell Ecosystem

## Hackage

<http://hackage.haskell.org>

main Haskell upstream package source repository

## Haskell packages

- `Cabal` packaging system

- `cabal-install` package tool

## Cabal packages

Package .cabal files have detailed metadata including dependencies allowing most packages to be packaged completely automatically

- Hackage has revisions of dependency bounds!

- [example package](https://hackage.haskell.org/package/cabal-rpm)

## Hackage

currently total nearly 13,000 packages!

(of course not all still maintained)

## Stackage

<http://stackage.org>

Stable consistent buildable Haskell package sets

Currently about 2300 packages

- [example package](https://www.stackage.org/package/cabal-rpm)

`stack` build tool

# Fedora Haskell

## Fedora Haskell SIG

Started in 2007

<https://fedoraproject.org/wiki/Haskell_SIG>

## Fedora Haskell challenges

currently ~480 Haskell sources packages in Fedora

GHC has strict version binary dependencies:

updating ghc or libraries requires a lot of rebuilding


## Fedora Haskell Packagers

Many people have contributed including:

- Currently active:
 Elliott\ Sales\ de\ Andrade,\
 Robert-André\ Mauchin, Jens\ Petersen

- Packagers:
 Ben\ Boeckel, Ricky\ Elrod, Zach\ Oglesby

<p style="font-size:0.9em">
Former:
 Bryan\ O'Sullivan, Yaakov\ Nemoy, Conrad\ Meyer, Lakshmi\ Narasimhan, Shakthi\ Kannan, Michel\ Salim, et al
</p>

# Tools for Fedora Haskell

# cabal-rpm

## [cabal-rpm](https://hackage.haskell.org/package/cabal-rpm)

RPM packaging tool

- converts Haskell package .cabal files to RPM .spec files
- can update packages and refresh packaging

OpenSuse has also used cabal-rpm

## Demo of cabal-rpm

# Fedora Haskell Tools

## fedora-haskell-tools

- fhpkg: clones, diff's, pulls, commits packages
- fhbuild: builds locally, in mock, Koji "chain" builds
- fhbz: closes Update bugs with builds or refreshes missing deps

## Demo of<br/>fedora-haskell-tools

# rpmbuild-order

## rpmbuild-order

New tool that orders packages by build dependencies

## Demo of rpmbuild-order

# Future plans

## Improvements to tools

- async parallel building
- containers for local building
- performance: caching of built package info
- editor for .spec files
- push the Haskell types

- querying for Fedora (Haskell) packages\
  (in the post-pkgdb world)

## Distro Haskell competition

- Nixos: all of Hackage!
- OpenSuse: Stackage LTS

## Continuous integration?

To keep Fedora in sync with Stackage LTS

Copr: build a subset of Stackage?

Koschei and Fedora CI

# Questions?

## Contact me

Mail: petersen@redhat.com

Twitter: @juhp

## More information

- Fedora Haskell SIG: <https://fedoraproject.org/wiki/Haskell_SIG>
- <https://github.com/juhp/cabal-rpm>
- <https://github.com/fedora-haskell/fedora-haskell-tools>
- <https://github.com/juhp/rpmbuild-order>
