---
title: fbrnch @ Nest with Fedora
author: Jens Petersen (@juhp) - 2020
patat:
    wrap: true
    margins:
        left: 10
        right: 10
...

```
  __ _                     _
 / _| |__  _ __ _ __   ___| |__
| |_| '_ \| '__| '_ \ / __| '_ \
|  _| |_) | |  | | | | (__| | | |
|_| |_.__/|_|  |_| |_|\___|_| |_|
```

a cli tool to ease Fedora Packager workflow


# Fed Brunch (pronounced "F br{a,u}nch")

fbrnch: Fedora branches packaging tool

- list, create and update _package reviews_ from spec files
    - repo request and import directly from bugzilla review
- build packages
    - merging branches & pushing updates to Bodhi
- handles both ***dist-git*** and ***non-distgit***:
    - koji scratch builds
    - prep/local/srpm/install/mock packages locally
- automated __parallel__ koji builds in dependency layers
- diff local changes (when changing many packages)
- show package status and bugs
- push incremental Copr builds

# History

- started off as "fedbrnch"
    - (harder to type and complete)
- simple initial goal of building fedora packages across branches
    - repetitive work which can be largely automated

- then added support for Package Review workflow
- and automatic bodhi updates

- many of the build related features were initially implemented in fedora-haskell-tools

# Use case: *Package Reviews* (1)

Package Review creation:

```
[pkg]$ fbrnch create-review
```

- check package name same as specfile
- pre-checks for any existing reviews
- creates srpm from spec file (prevents out of sync)
- runs `rpmlint` and optionally `mock`
- uploads spec & srpm and creates review bug (like fedora-create-review)

# Use case: *Package Reviews* (2)

Package Review update:

```
[pkg]$ fbrnch update-review
```

- creates srpm from spec
- runs `rpmlint` and optionally `mock`
- uploads spec & srpm and updates the review bug

# Use case: *Package Reviews* (3)

Package Reviews status:

```
$ fbrnch reviews
```
lists your open package reviews

Also: `find-review`

# Use case: *Building*

```
$ fbrnch build -B pkg1 pkg2 pkg3
```

builds packages over all their active branches

- offers to merge newer branch and push
- checks against existing Koji build
- checks for krb5 ticket
- creates Bodhi update (except Rawhide)
- optionally does override tag and waitrepo

```
$ fbrnch build -b master -b f32 mypkg
```

# Local actions (both dist-git and not)
```
[pkg]$ fbrnch local
```

```
$ fbrnch install pkg1 pkg2 pkg3
```

Other local actions:

- `clone`, `merge`, `commit`, `pull`
- `srpm`, `sort`, `prep`, `mock`, `install-deps`, `diff`


# Demos - done


# Full disclosure

fbrnch is written in Haskell

"Reads" (queries) Fedora services natively

- (pending release) pagure, koji, bodhi, copr
- (released) bugzilla-redhat

But currently still "writes" through the python clients

- fedpkg, bodhi, koji, copr


# Parallel building

- uses Haskell `async` library

- `rpmbuild-order` handles dependency graphs, sorting, and layers

```
$ fbrnch parallel -b master -t f33-build-side-345 *
```
sorts and builds packages in dependency layers


# You may also like

cobrnch for staggered copr builds

So I added it as a copr command to fbrnch

```
$ fbrnch copr -b master -b f32 project mypkg
```


# Thanks

😃 Thank you to the early adopters and testers:

- Tristan de Cacqueray (tristanC)
- Mike Fabian (mfabian)
- Elliott Sales de Andrade (qulogic)
- Dan Čermák (defolos)
- and others


# What's next

package fbrnch for Fedora

Moar parallel building

# More info

Source: <https://github.com/juhp/fbrnch>

You can try fbrnch today: `petersen/fbrnch` Copr repo

CI: <https://travis-ci.org/github/juhp/fedbrnch>

rpmbuild-order is in Fedora
