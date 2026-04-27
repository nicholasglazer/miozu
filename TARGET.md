# Miozu — target, positioning, 2026 plan

Author: Nicholas Glazer · Last revised: 2026-04-17

Living strategy doc. Supersedes prior ad-hoc notes. Revise when the
landscape or the plan shifts materially; don't let it decay quietly.

## Audience

Professional developers who live in Linux: power users, researchers,
founders, polyglot engineers. Overlap with Arch / NixOS / tmux+vim
communities. Not ricers. Not mainstream-DE users. Not newbies.

Think: the person who already has a tiling WM, already writes their
own config in dotfiles, already runs LLMs daily, and wants something
designed from the ground up for their workflow rather than adapted
to it.

## What the target wants (non-negotiables)

1. Diffable config — plain text, reviewable, portable between machines
2. Multiplex-native — tmux/vim muscle memory at the compositor level
3. LLM-as-primitive — not a popup overlay, a verb (Mod+P → fuzzy
   palette → MCP call → result)
4. Reproducibility — dotfiles OR Nix module; one-command new-machine
5. Observability — runtime IPC to introspect compositor state
6. Sub-10ms input latency — benchmarkable, published numbers
7. Session durability — reopen exactly where you left off
8. No forced chrome — no bubbles, no notifications unless opted in

## Moats (after 2026 competitive check)

MCP-compositor wrappers already exist in 2026: hyprmcp, kwin-mcp,
generic wayland-mcp. They call external IPC (`hyprctl`, KWin DBus).
So "first MCP-native DE" as a wrapper is taken. What is NOT taken:

1. **Terminal + compositor as one binary.** Nobody else has this —
   Sway uses foot, Hyprland uses kitty/alacritty, all separate
   processes. teru is miozu's primary client. Shared rendering
   stack, shared config, shared lifecycle. Structural, not
   marketing.

2. **MCP as first-class config primitive.** Not a wrapper calling
   `hyprctl` — MCP tools ARE the keybind verbs, bar data sources,
   session save/restore protocol, automation surface. Command
   palette (Mod+P) is a fuzzy UI over the MCP tool registry; LLM
   is one more invocation path, not a fallback. See
   `project_teru_config_architecture` (memory).

3. **Zig.** Memory safety without async runtime cost; zero hidden
   allocations (vs Rust std); clean wlroots C interop (vs C++ ABI
   pain). niri's Rust safety claim is the competition on this axis;
   Zig's "explicit everything" is the differentiator.

## Anti-goals

- NOT a distro — DE on top of any modern Linux
- NOT ricing — no eye-candy-first, no animation framework, no bling
- NOT mainstream — don't compete with GNOME/KDE polish
- NOT shell-locked — shell-agnostic by construction
- NOT opinionated on user taste — Dvorak/UA/Japanese workspaces are
  defaults for the author, overrides for everyone else

## Current state (2026-04-17)

- XMonad+X11 is the active daily driver
- teru terminal v0.4.1 stable
- miozu compositor in active development
  (`/home/ng/code/foss/teru/src/compositor/`)
- Single-user deployment (author's workstations)
- No package distribution, no docs site, no public users yet

## Order of operations

### Foundational (shell-agnostic, correctness)
- teruwm parses `environment.d/*.conf` at startup → drop shell-
  sourcing blocks from any *shipped* config
- Formal TOML schema + validator + useful error messages for
  `~/.config/teruwm/config`
- Bar as teru-embedded widget, first-class (scriptable but pre-
  configured out-of-box)
- `miozuctl` IPC surface — starting point for MCP-as-config
- Session save/restore protocol — dump state as JSON on signal,
  restore on next launch

### Pre-v1 (distribution, positioning)
- **Package before installer.** AUR `miozu-git`, then `miozu`.
  Nix flake + home-manager module
  (`programs.miozu.enable = true;`). Fedora COPR. Universal
  install.sh LAST.
- `miozu-session` launcher (`/usr/bin/miozu`, standard Wayland
  pattern — handles pre-flight, env loading as belt-and-suspenders,
  VT handoff, portal handshake)
- XDG portal backend (start with xdg-desktop-portal-wlr; swap in
  custom when teru integration matters)
- Notifications / clipboard / lock — pick mako / wl-clipboard /
  swaylock as defaults; custom implementations later if they pay
  for themselves
- miozu.com home page — positioning, one GIF of the MCP-keybind
  workflow, install commands

### Moat work (where it gets interesting)
- MCP-first config language: keybind RHS = local tool call OR
  remote MCP server. Bar widgets subscribe to MCP topics.
- Command palette (Mod+P) — fuzzy UI over MCP tool registry
- utsu capture integrated as built-in MCP verb
  (screenshot / gif / video → one keybind)
- Benchmark suite published on miozu.com with reproducible harness

## Distribution strategy

install.sh is fine for dogfooding. Professionals install via package
manager. Order, by expected adoption yield:

1. **AUR** — `miozu-git` (git head), `miozu` (stable). ~1 day of
   PKGBUILD work once v0.1 cut.
2. **Nix flake + home-manager module** — highest-quality adopter
   pool; the NixOS / home-manager crowd overlaps ~90% with target.
   ~1 week of module work.
3. **Fedora COPR** — mechanical once AUR exists.
4. **Universal install.sh** — distro-detecting wrapper, LAST.

## Repo split (post-v1)

- `~/.miozu/personal/` — author's taste (fish block, Dvorak+UA,
  Japanese workspaces, scratchpad layouts). Never shipped.
- `~/.miozu/defaults/` — reference configs shipped in AUR/Nix as
  `/usr/share/miozu/defaults/`. Users start from these and diff.

Professionals want to see defaults and customize from there, not
adopt a stranger's taste wholesale.

## 2026 landscape snapshot

Compositor field (competitors / neighbours):

- **Hyprland** (v0.48+) — ricer mindshare, heavy config, animation-
  first, C++ plugins
- **Sway** (1.10) — conservative, i3-compat, low-churn reference
  wlroots compositor
- **niri** — scrollable tiling (Rust), rising 2025–2026
- **river** — declarative/scriptable, small cult
- **COSMIC** (System76) — Rust, mainstream-leaning, TOML + GUI
  settings app
- **GNOME / KDE** — not the competition

MCP ecosystem (Apr 2026): ~10,000+ public MCP servers; adopted by
ChatGPT, Cursor, Copilot, AWS/GCP/Azure. MCP is table-stakes for
serious developer tooling, not a differentiator on its own.

Nix adoption: every modern compositor ships a home-manager module
(Hyprland, Sway, niri, Wayfire). The pattern is well-trodden — a
miozu module is not a research project.

## Reading list / memory cross-refs

- `project_compositor_plan` — full architecture plan
- `reference_teru_codebase` — module map
- `project_teru_config_architecture` — TERU_DEPTH nesting, MCP-as-
  keybind-RHS
- `project_miozu_features` — XMonad parity checklist
- `project_utsu` — capture MCP
- `project_architecture_decisions` — naming, repo structure, bar,
  config, rendering, NOT a distro
- `docs/KEYBOARD.md` — XKB precedence chain
