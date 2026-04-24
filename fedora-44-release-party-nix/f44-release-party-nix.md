% Adding `nix` to Fedora: we did a thing
% Jens Petersen  `<petersen@redhat.com>`\
  with Zbigniew Jędrzejewski-Szmek (`zbyszek`)
% _F44 Release Party, April 2026_\
  \
  \
  <small><https://petersen.fedorapeople.org/f44-release-party-nix.html>\
  <https://petersen.fedorapeople.org/f44-release-party-nix-single.html></small>


# What is Nix?

Sometimes used to refer to:

- `nix`, the tool
- nixpkgs
- nixos

## `nix` is
<https://github.com/NixOS/nix/>

- a package manager
- devel build/environment tool
- declarative functional programming language
- used for making deployments/containers, etc

\

- `nix` has both legacy and modern flake cmds
  - eg `nix-shell` vs `nix shell`
- Nix takes "builds on my machine" to the extreme

## nixpkgs
<https://github.com/NixOS/nixpkgs/>

large package collection (distribution)

- 120,000 or more packages

## nixos
OS (including installer with default config, etc)

# Packaging `nix`
difficult to package `nix`:

- somewhat exotic dependencies
  - lately moved from autotools to meson
- complicated to setup manually
  - needs system `nix-daemon` and `/nix` to be setup
  - so many people just run:\
  <small>`sh <(curl -L https://nixos.org/nix/install) --daemon`</small>

# Copr package
- `petersen/nix` copr repo: _popular_
  - <https://copr.fedorainfracloud.org/coprs/petersen/nix>
  - created in 2016, derived from other copr's

# Fedora Change

with @zbyszek

<https://fedoraproject.org/wiki/Changes/Nix_package_tool>

- zbyszek helped to review the package, which improved the packaging

# Adding `/nix`

zbyszek crucial contribution that elevated this Change was petitioning FeSCo
to allow `/nix` in Fedora with a special [exception](https://docs.fedoraproject.org/en-US/packaging-guidelines/#_exceptions_2), which was approved!

This helped make this Change a lot more meaningful.

# Upstream contributions needed
- soversion
- manpages

# Packages structure

- `nix` (metapkg)
- **`nix-core`**
- `nix-libs` and `nix-devel`

\

- `nix-daemon` (daemon files)
- `nix-filesystem` (`/nix`)
- `nix-legacy` (`/usr/bin/nix-*`)
- `nix-system` (`/nix/store`, `nixbld`)
- `nix-doc`

# Installation

- daemon mode vs single-user vs rootless

- containers vs package mode vs image mode

<https://src.fedoraproject.org/rpms/nix>

# nixGL flake

Allows using OpenGL apps (Mesa) and also Vulkan.

Now documented in the package readme:
<https://src.fedoraproject.org/rpms/nix>

# Current status

Fedora package is quite good state now, including gating tests.

- flakes and nix-command enabled

- f45/f44: nix-2.34
- f43/f42: nix-2.31
- epel10: nix-2.24

We have `#fedora-nix` on Matrix.

Good relationship with upstream

# What is next?

- add `/nix` bind mount
  - hopefully for F45
- configuration of `nix-nswrapper` (new in nix-2.34)
- maybe improve selinux handling

# Conclusions

- nix is available to install and try in Fedora
- I like my rpm packages, though nixpkgs does provide many packages

# Optional demo
