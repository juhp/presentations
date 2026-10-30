% Isolating tools\
  in the age of\
  agentic development
% Jens Petersen (Red Hat)
% GNOME Asia 2026\
  Terengganu, Malaysia

# Talk Structure

- Background: the Problem
- The need for isolation: different approaches
- Using containers to _"`encapsule`"_ projects
- Linux namespaces with Bubblewrap: _"`workcell`"_ approach
- Related project and future ideas

Note the basic target of this talk is Linux systems.

::: notes
~45 min. Four parts: the problem,
encapsule (containers), workcell (bubblewrap), conclusions.
Both tools are mine, both are best-effort: "better than nothing".
:::

# 1a. The problem

---

## Background story

For a long time I felt uncomfortable running\
installer shell scripts directly from Github projects\
or companies' websites to install their tools\
or binary bundles.

::: notes
Sometimes I run "curl | sh" myself, and so do you probably
:::
## The problem

We are expected to run\
`curl | sh` \
but I prefer not to do that with\
access to my home directory.

::: notes
i.e. we are executing potentially arbitrary shell script.
*Pause here*.
Everything on your computer that you can read, this code can read.
:::

---

I work on Fedora and RHEL OS development at Red Hat,\
so it is really important that I keep\
my own home directory & environment safe and clean.

---

Finally I decided to write `encapsule` and\
then `workcell` which I am presenting today.


# 1b. The modern installer culture

```sh
curl -fsSL https://example.com/install.sh | sh
```

---

## Shell installers are so common

Here are a few well-known examples:

---

Coding agents

- Opencode: <https://opencode.ai/>
- Pi agent: <https://pi.dev/>

Languages

- Rust: <https://rust-lang.org/learn/get-started/>
- Lean4: <https://lean-lang.org/install/manual/>

---

Installation of package managers

- Nix <https://nixos.org/download/#nix>
- Homebrew: <https://brew.sh/>
- Bun <https://bun.com/>

Local AI tools

- Ollama: <https://ollama.com/download>
- [Llama.cpp](https://github.com/ggml-org/llama.cpp#quick-start)

etc

---

Note: _not saying any of these examples_ are _problematic_

::: notes
Not picking on any of these projects: they are all convenient and mostly fine.
The structural issue: the thing you execute is fetched fresh every time, so
its integrity depends on the server, every time.
:::

---

Note that installing software with\
package managers also *runs* code.

<small>
(and if your install script also git clones arbitrary code\
from HEAD it doesn't make me feel any better ;o)
</small>

So the problem is wider than `curl | sh`

::: notes
npm preinstall/postinstall hooks, pip setup.py / build backends,
cargo build.rs, Makefiles, git hooks, editor plugins.
Not only curl | sh.
:::

---

::: big
Now add _coding agents_ into the mix that potentially\
may run arbitrary commands **for you** !
:::

::: notes
Agents read files, run shell commands, install dependencies, and may be
prompt-injected by content they read. Many people run them in YOLO /
skip-permissions mode because the prompts are tiresome.
:::

---

Where to draw the line of trust?

---

::: big
What attackers are after:

access to your **`$HOME`** !
:::

::: notes
SSH keys, Cloud & AI keys and other account credentials (.env files),
browser profiles, shell history, wallets.
Filesystem (home) isolation is the key feature.
:::

## What we usually do instead

- Read the install script first (but do you really?)
- Pin versions and verify checksums\
  (good, but not always possible)
- "Only use trusted sources" (preferred approach)
- Hope and :pray:!

. . .

They focus on trying to **find** any problem,\
but do not limit any actual **damage** if it happens.

---

The focus today is more

::: big
**Containment** rather than analysis
:::

## The isolation spectrum

<pre class="diagram">
less isolation                               more isolation

plain user --> namespaces --> containers --> VMs
(toolbox)      (bubblewrap)   (podman)       (kernel)

       light, fast --------------------> heavy
</pre>

. . .

This talk: the **two middle** ones

::: notes
Containers vs VMs: VMs are the strongest boundary but heavy and awkward for
GUI/dev workflows. Containers and namespaces share the host kernel:
a kernel bug can escape them. That is the trade-off we accept in exchange
for convenience.
:::

---

::: big
Goal: Basic isolation - *"better than nothing"*,\
easy enough that you actually use it.
:::

::: notes
Explicit opt-in: nothing from the host is visible unless you grant it.
Not claiming a hardened security boundary.
:::

# 2. Encapsule

---

::: big
Containers,
but **without** your home.
:::

## First: what is toolbox?

```
$ toolbox create
$ toolbox enter
```

- A pet container (podman) for development
- Much like your host: same user, same `$HOME`,\
  same display, same sockets
- But _may_ be a different version or OS to the host!
- Designed for **integration**, not isolation
- On Fedora, toolbox uses `fedora-toolbox` images by default

Another similar tool is _Distrobox_.

::: notes
Quick intro for the non-toolbox audience. It is great for keeping dev packages
off an immutable host (Silverblue/Kinoite etc.) - but it bind-mounts your
home directory, so inside it the malicious script sees everything.
:::

---

::: big
Toolbox shares your home
**by design.**
:::

---

Recap: what is a container? (in half a slide)

```sh
$ podman run --interactive --tty --rm fedora:latest
```

runs the container _image_ as a (temp) container

Containers can be started, stopped like VMs can.

## Encapsule

Run a toolbox (or any) image as an **isolated** podman container

- No `$HOME` bind-mount, no host integration by default
- You explicitly choose: **project dir**, **temp home**, **capabilities**
- Originally derived from `toolbox-constrained` (python)
- Written in Haskell
- Packaged in Fedora (F45 Change)

```sh
$ encapsule run fedora-toolbox:45 --home ~/isolated --project ~/src/abc
```

::: notes
"Encapsule container" = the container; "encapsule" = the tool.
Based on https://github.com/swick/toolbox-constrained.
:::

## Principles

- **Only what you ask for is shared**
- Container is **you** inside (`--userns=keep-id`), not root
- No working `sudo` unless you ask (`--sudo`),\
  but can use `--root` to gain root access
- Your real home: **invisible** by default

---

::: big
Demo 1
:::

## Encapsule basics

```shellsession
$ encapsule run fedora-toolbox:45
[petersen@encapsule-fedora-toolbox-45 ~]$ ls -a ~
.  ..  .bashrc  .bash_logout  .profile
[petersen@encapsule-fedora-toolbox-45 ~]$ ls ~/.ssh
ls: cannot access '/home/petersen/.ssh': No such file or directory
```

::: notes
TODO: replace with a real recorded session (asciinema or screen recording).
The output above is illustrative; check the real prompt and skel files.
Fallback for the podium: play the recording instead of running it live.
:::

## Your project, your home

```sh
# Temporary container, removed on exit
$ encapsule run fedora-toolbox:45 --project $PWD

# Reusable container for a project
$ encapsule create fedora-toolbox:45 -p ~/src/abc --home ~/isolated
$ encapsule enter fedora-toolbox-45-abc

# Turn an existing toolbox container into an encapsule image
$ encapsule commit fedora-toolbox-45
```

<small>

- `-p` (--project): writable, and the working directory
- `-H` (--home): a separate, empty (or persistent) home
- `--readonly`, `--no-network` for more locking down

</small>

::: notes
`run` = temporary, `create` + `enter` = long-lived. `commit` reuses a toolbox
you already set up as the base fs (your original toolbox is untouched, and its
home bind-mount is NOT carried over).
:::

## Capabilities

Named, opt-in grants in `~/.config/encapsule/config.toml`

```toml
[capabilities.ssh]
volumes = ["~/.ssh:~/.ssh:ro"]

[capabilities.git]
volumes = ["~/.gitconfig:ro"]

[capabilities.wayland]
env = ["WAYLAND_DISPLAY", "XDG_RUNTIME_DIR"]
volumes = ["$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY"]
```

```
$ encapsule create dev --cap git --cap wayland
```

::: notes
No capability is enabled by default. Note: unrelated to Linux capabilities(7).
Each can also set: path, init, security_opts, podman_opts.
:::

---

::: big
Example (demo?)
:::

## A "malicious" script

```sh
$ cat evil.sh
#!/bin/sh
ls -d ~/.ssh ~/.aws ~/.config/gh 2>&1
env | grep -ci 'token\|key\|secret'

$ sh evil.sh                              # on the host
/home/me/.aws
/home/me/.ssh
4

$ encapsule run fedora-toolbox:45 -p . -- sh evil.sh
ls: cannot access '/home/me/.ssh': No such file or directory
ls: cannot access '/home/me/.aws': No such file or directory
0
```

::: notes
TODO: record for real with encapsule (podman was not available in my test
environment, so the encapsule output above is what I expect, not captured).
The host output is from a real run with decoy ~/.ssh and ~/.aws in a throwaway
HOME and a fake GITHUB_TOKEN. The script only lists, never uploads anything.
Show the host first, then the same script in the container.
:::

## Under the hood

- It is **just `podman run`** with the right flags: `--dryrun` prints it
- `--userns=keep-id`, then drop privileges inside the container
- Bind mounts get SELinux `:z` shared labels by default
- Container named after the project: many at once

---

What it expands to:
```sh
~/src/workcell$ encapsule run --dryrun fedora-toolbox:45 -p .
==>
podman run --rm -it --userns=keep-id --name encapsule-fedora-toolbox-45-workcell\
--hostname encapsule-fedora-toolbox-45-workcell\
-e TERM -e COLORTERM -e LANG=C.UTF-8 -e=HOME=/home/petersen\
--passwd-entry 'petersen:*:3239:3239::/home/petersen:/bin/sh' --user=root\
--workdir /home/petersen/src/workcell -v /etc/localtime:/etc/localtime:ro\
-v /home/petersen/src/workcell:/home/petersen/src/workcell:z fedora-toolbox:45\
sh -c ''mkdir '\''-p'\'' '\''/home/petersen'\'' > '\''/dev/null'\'' 2>&1;\
chown petersen '\''/home/petersen'\'' > '\''/dev/null'\'' 2>&1;\
if ( test \( -d '\''/home/petersen'\'' \) &&  test \( -d '\''/etc/skel'\'' \));\
then :; runuser '\''-u'\'' petersen '\''--'\'' cp '\''-a'\'' '\''--update=none'\''\
'\''/etc/skel/.'\'' '\''/home/petersen/'\''\ > '\''/dev/null'\'' 2>&1; fi\
&& exec runuser -u petersen -- env HOME=/home/petersen bash''
```

::: notes
Don't go deep. The point is that it is a thin, auditable wrapper.
:::

## Encapsule: good

- A **complete distro** inside: reproducible, pick any Fedora/Ubuntu/...
- Familiar container tooling
- Networking, SELinux, and rootless isolation come from podman
- Same image for many projects

## Encapsule: not so good

- **Heavy**: a toolbox image can be several GB (mine: 6 GB)
- **A second copy** of your system: updates and drift
- Your host's installed tools are **not** there; set up again in the image
- Slower to start, and nested sandboxing is awkward

---

::: big
It is **not** a full security boundary,
:::

but provides some basic best-effort isolation.

::: notes
From the README disclaimer: simple, best-effort isolation, no warranty.
- Shared kernel
- Outbound network is open by default (use --no-network)
- Whatever you grant via capabilities is visible
Both tools are for reducing blast radius, not for running real malware.
:::

# 3. Workcell

---

::: big
What if we don't copy the system,
but **borrow** it?
:::

## Workcell _(working title)_

Run programs with the **host's own tools**, read-only,\
in a bubblewrap sandbox

- Read-only system (`/usr`, `/etc`)
- **Empty, isolated home** by default
- Writable **project** directory
- Private PID namespace, optional private network

```
$ workcell run
```

::: notes
Codename, not final. Short, easy to type and complete.
Originally planned as the bubblewrap experiment for within toolbox:
"use the enclosing system's installed tools, isolate home, select a project writable".
:::

## Bubblewrap

- Unprivileged sandboxing with Linux namespaces
- Small setuid-free helper, no daemon, no image
- It is what **Flatpak** uses under the hood

. . .

Workcell is a friendly CLI around it, written in Haskell

::: notes
bwrap is the low-level tool: very flexible, but you assemble the command
line yourself. `workcell run --dryrun` prints the bwrap command it would run.
:::

## What the process sees

<pre class="diagram">
 host                       workcell
 ----                       --------
 /usr, /etc          -->    /usr, /etc            (read-only)
 ~/                  -X     ~/  (empty, temporary)
 ~/src/project       -->    ~/src/project         (writable)
 /var /opt /srv /sys -X     (hidden)
 all processes       -X     own PID namespace
 network             -->    private (pasta)
</pre>

::: notes
Read the diagram left to right. Home is mounted at the original path so tools
behave normally, but it is empty. Explicit -v mounts add things back.
:::

---

::: big
Demo 3
:::

## Workcell: the same "installer"

```
$ sh evil.sh                     # on the host
/home/me/.aws
/home/me/.ssh
4                                # env vars matching token/key/secret

$ workcell run -- sh evil.sh
ls: cannot access '/home/me/.ssh': No such file or directory
ls: cannot access '/home/me/.aws': No such file or directory
0

$ workcell run -- touch /usr/x
touch: cannot touch '/usr/x': Read-only file system
```

- No image to pull, no container to create: **~60 ms** to start
- Uses the tools **already installed** on the host

::: notes
Real output from a test run (decoy ~/.ssh and ~/.aws in a throwaway HOME, and a
fake GITHUB_TOKEN in the environment). Paths shortened to /home/me for the slide.
Inside, only 5 processes are visible and the environment has about 8 variables.

`ls -A ~` inside shows only `src`, because the project directory lives under home.

If private networking (pasta) is unavailable, e.g. no /dev/net/tun, workcell
prints "falling back to host networking" - don't be surprised on stage.
Best candidate for a live demo: no network or image needed.

About 6x faster than encapsule
:::

## The project: three mounting modes

```sh
$ workcell run [-p ~/src/abc:rw]      # cwd, writable
$ workcell run -p ~/src/abc:ro -- git diff
$ workcell run -p ~/src/abc:O  -- make
```

```sh
$ workcell run -p .:ro -- sh -c 'echo >> README'
sh: line 1: README: Read-only file system

$ workcell run -p .:O -- sh -c 'echo x > new.txt; ls'
README  evil.sh  new.txt
$ ls                            # after exit: new.txt is gone
README  evil.sh
```

- `:rw` normal (default)
- `:ro` read-only: let a tool look but not touch
- `:O` **overlay**: run `make` or an agent, then **discard** all changes

::: notes
:O is nice for letting an unknown tool run and then throwing the writes away.
Requires bubblewrap overlay support and kernel/filesystem support.
:::

## Adding things back, explicitly

```sh
$ workcell run -v ~/.gitconfig -- git status      # read-only by default
$ workcell run -v ~/.cargo:rw -- cargo build
$ workcell run -H ~/sandbox-home                  # persistent home
$ workcell run -e LANG -e MY_VAR=hello -- env
```

- Environment is **cleared** except for a few safe variables
- Secrets in your shell environment do not leak in

## Network

| Flag | What you get |
|---|---|
| (default) | Private network via **pasta**, falls back to host with a warning |
| `--network-private` | Private, or fail |
| `--network-none` | Loopback only |
| `--network-host` | Share the host network |

<small>

- Private network separates **localhost** services (e.g. databases, dev servers)
- It does _not_ block outbound connections

</small>

::: notes
Destination filtering and port publishing are not implemented yet; the
README says so. That is the main gap vs. stronger tools like OpenShell.
:::

## GUI apps and the desktop

```sh
$ workcell run --wayland -- gtk4-demo
$ workcell run --x11 -- xterm
$ workcell run --wayland --dbus -- gtk4-demo
```

- `--wayland`: just the one socket
- `--x11`: broad access to other X clients, including input and clipboard
- `--dbus`: the **whole** session bus, unfiltered

::: notes
Each of these opens a hole by design. A D-Bus proxy is on the TODO list.
Audio and GPU are not exposed yet.
:::

## Encapsule vs workcell

| | encapsule | workcell |
|---|---|---|
| Mechanism | podman container | bubblewrap |
| System | image copy (GBs) | host, read-only |
| Setup | pull / commit image | none |
| Start | quick | instant |
| Home | isolated | isolated |
| Network | podman | pasta, none, host |
| Tooling | any distro | what the host has |

::: notes
Neither replaces the other. Encapsule when you want a different distro or a
reproducible image; workcell when you want your host's tools with less ceremony.
:::

---

::: big
Also **not** a sandbox
for untrusted code.
:::

::: notes
From NOTES.md: "This is a filesystem isolation experiment, not a sandbox for
untrusted code." /etc remains readable, private networking allows outbound,
explicit mounts and compat options can weaken isolation. Syscall filtering,
named capabilities and resource limits are not done yet.
:::

# 4. Conclusions

---

::: big
Isolate your **home**.

Share **explicitly**.
:::

## Take-aways

- `curl | sh`, packages, and agents all run **as you**
- The prize is your **home directory** and your secrets
- Containment can beat inspection
- Encapsule: a whole container; workcell: borrow the host. Both **opt-in**
- Both are **best effort**, but rather better than nothing

## Future

- **Workcell**: named capabilities, syscall filtering (seccomp), resource limits, D-Bus proxy
- **Encapsule**: landlock, per-project config (`.encapsule.toml`), maybe an OpenShell mode
- Shared capability ideas between the two tools
- Stronger options for real malware: VMs
- Tool around capturing `curl | sh` outputs?

## Related projects

- [toolbox](https://containertoolbx.org/) and [toolbox-constrained](https://github.com/swick/toolbox-constrained)
- [schupfn](https://github.com/whot/schupfn/): toolbox image in a QEMU VM
- [podenv](https://github.com/podenv/podenv): declarative containerised apps
- [litterbox](https://github.com/Gerharddc/litterbox): more features, supports landlock
- [OpenShell](https://github.com/NVIDIA/OpenShell/): stronger sandboxing, esp. network
- [Flatpak](https://flatpak.org/) and [bubblewrap](https://github.com/containers/bubblewrap)

## Thanks

- encapsule: <https://github.com/juhp/encapsule> (Apache-2.0, in Fedora)
- workcell: <https://github.com/juhp/workcell> (LGPLv2.1+, WIP)
- Slides: <https://juhp.fedorapeople.org/talks/gnome-asia-2026-isolation/>

Jens Petersen (@juhp)

Questions?

::: notes
TODO: check the slides URL. The Makefile `upload` target uses
fedorapeople.org:talks/gnome-asia-2026-isolation/.
:::
