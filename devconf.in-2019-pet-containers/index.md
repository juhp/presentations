% ![](20110717032101!Fedora_infinity.png "Fedora Logo"){ width=128 }\
  Pet Containers\
  in Fedora
% Jens Petersen\
  <petersen@redhat.com>
% Devconf.in 2019, Bangalore (2 Aug)

# Fedora Silverblue

##
![](20110717032101!Fedora_infinity.png "Fedora Logo"){ width=300 }
![](silverblue-logo.svg "Fedora Silverblue"){ width=256 }

## What is Fedora Silverblue?

- immutable OS: `/usr` is read-only
- uses [rpm-ostree](https://github.com/projectatomic/rpm-ostree) instead of dnf
- image deployments: atomic updates

## Adding software to Fedora Silverblue

- package layering (rpm-ostree)
- Flatpaks
- containers?


# Developing on Silverblue in Toolbox

## 

![](toolbox-wb.svg "Toolbox"){ width=640 }

## Toolboxes

- [debarshiray/toolbox](https://github.com/debarshiray/toolbox)

- [cgwalters/coretoolbox](https://github.com/cgwalters/coretoolbox)

- [coreos/toolbox](https://github.com/coreos/toolbox)


# Live Demo


# How does it work?

- analysis of the shell script
- container image


# Pros and cons

## Misc

- really like having a Rawhide container environment for development
- using Toolbox for devel (maybe VM for testing)
- save disk-space
- can run apps from toolboxes on the desktop
  - eg I run Emacs

## Problems
- podman
- moving targets
- getting accustomed
- Mock/ping only work in host


# Future ideas
- Sharing fonts with host
- Golang rewrite?


# What questions do you have?

## Contact

Mail: petersen@redhat.com

Twitter: @juhp

<https://petersen.fedorapeople.org/talks/devconf.in-2019-pet-containers>

*(slides created with Pandoc)*

## More information
<https://silverblue.fedoraproject.org/>

<https://github.com/debarshiray/toolbox>

<https://podman.io/>

<https://flatpak.org/>


