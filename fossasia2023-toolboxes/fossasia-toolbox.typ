#import "typst-slides/slides.typ": *


#show: slides.with(
    authors: [Jens Petersen `@juhp`\
              `petersen@redhat.com`
              ],
    short-authors: "Jens Petersen @juhp",
    title: "Living in toolbox containers",
    subtitle: "FOSSASIA OpenTech Summit 2023, Singapore",
    short-title: "Living in toolbox containers",
    date: "2023-04-15 ",
)

#new-section("Toolboxes")

#slide(theme-variant: "wake up")[
    = Toolboxes
]

#slide(title: "What is a Toolbox container?")[
    #one-by-one[
    ][
    - linux rootless pet containers
    ][
    - running over Podman or Docker
    ][
    - shares /home and host resources
    ]
]

#slide(title: "Toolbox projects")[
    currently 2 major projects:

    - Toolbx

    - Distrobox

    #text(size:13pt)[(There is/was also CoreOS toolbox)]
]

#new-section("Toolbx")

#slide(theme-variant: "wake up")[
    #link("https://github.com/containers/toolbox")
    #image("toolbox-logo-landscape.svg")
]

#slide(title: "Toolbx")[
    #one-by-one[
    - started in Red Hat by Debarshi Ray (rishi)
    ][
    - originally shell scripts
    ][
    - rewritten in Golang by Ondřej Míchal (HarryMichal)
    ][
    - Available as:
      - `toolbox` in Fedora, RHEL, Arch
      - `podman-toolbox` in Debian & Ubuntu
    - https://containertoolbx.org/
    - https://repology.org/project/toolbox-development-environment/versions]
]

#slide(title: "Toolbx images")[
    Toolbox needs custom toolbox container images:
    - `registry.fedoraproject.org/fedora-toolbox:{36,37,38,39}`

#link("https://github.com/toolbx-images/images")

    - #link("https://quay.io/organization/toolbx-images") has images for:\
    alpine, almalinux, amazonlinux, archlinux, centos, debian, rockylinux, opensuse, rhel, ubuntu
]

#slide(title: "Toolbx under the hood")[
]

    #new-section("Distrobox")

#slide(theme-variant: "wake up")[
    = Distrobox
    #image("distro-box.png")
]

#slide(title: "Distrobox")[
    #link("https://github.com/89luca89/distrobox/")

    - collection of shell script commands: `distrobox-*`
    - #link("https://github.com/89luca89/distrobox/blob/main/docs/compatibility.md")
    - #link("https://repology.org/project/distrobox/versions")
]

#new-section("Toolboxes")

#slide(title: "Thanks!")[
    Toolbox projects:
    + Toolbx: https://github.com/containers/toolbox
    + Distrobox: https://github.com/89luca89/distrobox/

    Contact me:
    - `@juhp` (github, twitter, mstdn.social)
    - `petersen@redhat.com`
    - slides: #link("https://github.com/juhp/presentations/tree/master/fossasia2023-toolboxes/") written in Typst
    ]
