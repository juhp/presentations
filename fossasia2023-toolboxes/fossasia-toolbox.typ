#import "typst-slides/slides.typ": *


#show: slides.with(
    authors: [Jens Petersen `@juhp`\
              `petersen@redhat.com`\
              Red Hat Asia-Pacific
              ],
    short-authors: "Jens Petersen @juhp",
    title: "Living in toolbox containers",
    subtitle: "FOSSASIA OpenTech Summit 2023, Singapore",
    short-title: "Living in toolbox containers",
    date: "2023-04-15 ",
)

#show link: underline

#new-section("Container Toolboxes")

#slide(title: "What is a Toolbox container?")[
    #one-by-one[
    ][
    - Linux pet container for a user
    ][
    - running in Podman or Docker
    ][
    with access to home dir, desktop and system resources of your host system
    ]
]

#slide(title: "Container Toolboxes")[
    4 fundamental questions:

    - Why?
    - How?
    - What?
    - Who?
]

#new-section("Why Toolboxes")

#slide(theme-variant: "wake up")[
    = Why Toolboxes
]

#slide(title: "Why use Toolboxes?")[
    - system package isolation
    - "immutable" ostree-based OS

    - use a newer/older OS version or different Linux distro
      - use different versions of apps, tools, toolchains, etc

    - lightweight and resource saving/sharing
]

#new-section("How Toolboxes")

#slide(theme-variant: "wake up")[
    = How Toolboxes
    #align(center)[#image("Podman.png", width: 50%)]
]

#slide(title: "How do Toolboxes work?")[
    - standard (rootless, privileged) user containers

    - use namespaces and bind mounts to make host system resources available

    - usually two step process: `create` and `enter` (or `run`) toolbox

    (This topic would make an interesting deep dive.)
]

#new-section("What Toolboxes")

#slide(theme-variant: "wake up")[
    = What Toolboxes
]

#slide(title: "Toolbox projects")[
    currently 2 major projects supporting graphical desktop users:

    - Toolbx

    - Distrobox
]

#new-section("Toolbx")

#slide(theme-variant: "wake up")[
    #link("https://github.com/containers/toolbox")
    #image("toolbox-logo-landscape.svg")
]

#slide(title: "Toolbx")[
    #one-by-one[
    - started in Red Hat by Debarshi Ray (rishi) in 2018
    ][
    - originally shell script and uses Podman
    ][
    - rewritten in Golang by Ondřej Míchal (HarryMichal) in 2020
    ][
    - Available as:
      - `toolbox` in Fedora, RHEL, Arch
      - `podman-toolbox` in Debian & Ubuntu
    - #link("https://containertoolbx.org/")
    (#link("https://repology.org/project/toolbox-development-environment/versions")[repology.org])
    ]
]

#slide(title: "Toolbx images")[
    Toolbox needs custom toolbox container images:
    - `registry.fedoraproject.org/fedora-toolbox:{36,37,38,39}`
    - `registry.access.redhat.com/ubi{8,9}/toolbox`

Others in #link("https://github.com/toolbx-images/images") (created in 2022)

    - #link("https://quay.io/organization/toolbx-images")
    - has images for:\
    alpine, almalinux, amazonlinux, archlinux, centos, debian, rockylinux, opensuse, rhel, ubuntu
]

#slide(title: "Toolbox demo")[
]

#new-section("Distrobox")

#slide(theme-variant: "wake up")[
    = Distrobox
    #image("distro-box.png")
]

#slide(title: "Distrobox")[
    #link("https://github.com/89luca89/distrobox/")

    community project created by Luca Di Maio in 2021
    - collection of shell script commands: `distrobox-*`
    - supports Podman and Docker
    - #link("https://github.com/89luca89/distrobox/blob/main/docs/compatibility.md")
    (#link("https://repology.org/project/distrobox/versions")[repology.org])
]

#slide(title: "Distrobox demo")[

    - `create --home`
    - `--dry-run`
    - `export --app` to add .desktop file to host desktop
    - `ephemeral`
]

#new-section("Who Toolboxes")

#slide(theme-variant: "wake up")[
    = Who Toolboxes
]

#new-section("Who Toolboxes")

#slide(title: "Who Toolboxes")[
    - developers
    - testers
    - power users
    - system troubleshooting
]

#slide(title: "Thanks!")[
    Toolbox projects:
        + Toolbx: https://github.com/containers/toolbox
        + Distrobox: https://github.com/89luca89/distrobox/

    Contact me: Jens Petersen
        - `@juhp` (github, twitter, mstdn.social)
        - `petersen@redhat.com`

        Slides: #link("https://github.com/juhp/presentations/tree/master/fossasia2023-toolboxes/") (written in Typst)
    ]
]
