# Miozu — Positioning & Shipping Plan

Reality-checked on 2026-04-13. Supersedes the earlier ultraplan draft, which
confused `teruwm` with an X11 WM and `gnosis-mcp` with a desktop-control MCP.
Neither is true.

## The real stack (what exists today)

| Component | What it actually is | Status | Repo |
|---|---|---|---|
| **teru** | Terminal emulator + multiplexer (Zig 0.16) | v0.4.24, stable | `github.com/nicholasglazer/teru` |
| **teruwm** | Wayland compositor built on wlroots, embeds libteru, has `WmMcpServer` for AI desktop control | In development, same repo as teru (`src/compositor/`) built with `-Dcompositor=true` | same |
| **gnosis-mcp** | Docs / knowledge-base RAG MCP server (Python, on PyPI). `search_docs`, web crawl, git history. Running on prod server. | Shipping on PyPI, competitor to Context7 | `github.com/nicholasglazer/gnosis-mcp` |
| **jera** | Svelte 5 component library, open source. Foundation for selify.ai and teru.sh. | Active | `github.com/nicholasglazer/jera` |
| **hsrx** | Haskell X11 screen recorder (active-monitor capture) | Niche tool, useful for demo GIFs | `github.com/nicholasglazer/hsrx` |
| **miozu (this repo)** | Dotfiles + installer + theme + Wayland session glue | Transitioning from XMonad → teruwm | `.miozu` |
| **selify.ai** | Stealth SaaS startup (uses jera) | Revenue engine | private |

Domains owned: `teru.sh`, `miozu.com`, `gnosismcp.com`, `upgo.ca`, `nicgl.com`,
`selify.ai`.

## Positioning

**miozu** = the AI-first Linux desktop *distribution-of-dotfiles*. Not a distro
(per architecture decision 2026-04-11). A curated, installable DE for
professionals who want a lightweight, keyboard-driven, agent-controllable
workstation.

One sentence: **"An AI-first desktop for professionals — tiling Wayland
compositor with an MCP server baked in, local-LLM friendly, under 100MB RAM
idle."**

Three audiences, in order of priority:

1. **Individual power users / developers** who already run tiling WMs (XMonad,
   i3, Hyprland) and want a modern Wayland replacement with AI hooks. Primary
   distribution: dotfiles repo + Arch install script. Free.
2. **AI tool builders** who want a desktop their agents can drive.
   `teruwm`'s MCP server exposes windows/workspaces/spawn; `gnosis-mcp` gives
   them a searchable docs backend. Cross-sell vector.
3. **Small studios / solo consultants** who'd pay for a hardened, supported
   version (managed configs, remote onboarding). Revenue path, later.

## What is NOT miozu

- Not a Linux distribution. Arch + dotfiles. Refuse distro forks.
- Not a launcher/shell replacement a la "bobshell". That was a voice typo in
  the original brief.
- Not a GNOME/KDE competitor. Tiling-only, no floating-desktop workflows.
- Not tied to one LLM provider. Works with Ollama, Claude, OpenAI, whatever
  speaks MCP.

## Ecosystem map

```
                       miozu.com (brand hub)
                              │
        ┌─────────────────────┼─────────────────────────┐
        │                     │                         │
    teru.sh              gnosismcp.com             nicgl.com (personal)
    (terminal)           (docs RAG MCP)
        │                     │
        └────────┬────────────┘
                 │
            shared: Claude-Code / Cursor / Ollama / any MCP client
                 │
        ┌────────┼─────────────┐
        │        │             │
    teru      teruwm       jera (Svelte 5 UI lib) ──► selify.ai (stealth)
                                                 ──► teru.sh site
                                                 ──► miozu.com site
                                                 ──► gnosismcp.com site
```

jera is the quiet keystone: every marketing site shares one component library,
so brand visuals stay consistent and shipping new sites is cheap.

## Monetization

### Direct (deferred; not the focus right now)
- **Miozu Pro** (if demand appears): paid tier with managed config sync,
  priority support, extra themes. SaaS-style.
- **gnosis-mcp Cloud** (natural extension): hosted RAG for teams that don't
  want to run Python locally. Undercut Context7 on price.
- **Support contracts** for studios adopting miozu on fleet machines.

### Indirect (the real short-term play)
- **Fame / hiring signal.** A beautiful AI-first desktop on HN is worth more
  than $10k/mo MRR when you're running selify.ai stealth — it builds
  credibility for every other product you ship.
- **Funnel into selify.ai.** If miozu gains even 500 professional users, many
  will be potential selify customers.
- **Gnosis-mcp PyPI downloads → inbound leads** for gnosis-mcp Cloud when it
  launches.
- **jera adoption.** Every miozu/teru/gnosis site showcases jera; devs who
  like the aesthetic check the lib.

The pattern: monetize selify.ai, let miozu/teru/gnosis be the credibility
flywheel that makes selify sales easier and hiring trivial.

## What to ship, in order

### Phase 0 — positioning hygiene (this week)
- Rewrite `.miozu/README.org` with the positioning above. Include install
  command, feature list, screenshot, links to teru + gnosismcp.
- Update `miozu.com` (SvelteKit, presumably same stack as teru.sh) with the
  same headline. Pull hero screenshots from `hsrx` recordings.
- Cross-link: `teru.sh` links to `miozu.com` and vice versa. Both link to
  `gnosismcp.com`.

### Phase 1 — installable miozu session (next 2–4 weeks)
- Keyboard / locale fix via `~/.config/environment.d/` (shipping today, see
  Task 2).
- `bin/install-miozu-session.sh` that: (a) installs teru/teruwm/gnosis-mcp
  from prebuilt binaries or source, (b) writes `~/.config/miozu/` with the
  shared `miozu.conf` palette + `keybinds.conf`, (c) registers a Wayland
  session entry `/usr/share/wayland-sessions/miozu.desktop`.
- Document default keybinds against Dvorak (since that's the primary
  layout). Provide a QWERTY `keybinds.conf` as sibling.

### Phase 2 — AI-configurable (month 2)
- `~/.config/miozu/ai.conf` — pick `ollama | claude | openai`, pick default
  model, pick MCP servers to auto-launch. teruwm reads this on startup and
  wires `WmMcpServer` endpoints.
- Bundle `gnosis-mcp` as a default MCP so miozu ships with docs search out
  of the box.
- Optional: bundle a small `ollama pull qwen2.5-coder:3b` on first run if the
  user picks local-only mode.

### Phase 3 — packaging (month 3+)
- AUR packages: `teru`, `teruwm`, `miozu-dotfiles-git`.
- Publish demo video (record with `hsrx`) showing an agent driving the
  desktop end-to-end.
- Submit to HN / r/unixporn / r/linux.

## Config model (the "configurable for others" part)

Reuse teru's `include` directive — no new format.

```
~/.config/miozu/
├── miozu.conf        # palette + shared settings (single source of truth)
├── teru.conf         # includes miozu.conf + terminal keys
├── teruwm.conf       # includes miozu.conf + compositor keys
├── keybinds.conf     # shared; included by both
├── ai.conf           # LLM provider, MCP endpoints, model defaults
└── themes/           # optional alt palettes (nord, gruvbox, tokyonight)
```

Third-party users override by dropping files in `~/.config/miozu/` — no
`miozu` wrapper CLI needed for v1. If demand appears, add
`miozu theme set <name>` later.

## Dependencies policy

Inherit the existing rules from `.claude/rules/zig-compositor.md`:
- teru + teruwm: no new Zig deps without discussion. Wayland adds exactly one
  system dep: `wlroots-0.19`. Done.
- Codecs/drivers: whatever Arch pulls in as `wlroots` transitive deps
  (libdrm, libinput, pixman, libseat, mesa). Acceptable.
- gnosis-mcp: Python, lives in its own repo with its own dep story.
- jera: separate Svelte 5 / SvelteKit story, not coupled.

## Marketing assets needed

- [ ] miozu.com hero video (30s, screen-recorded with hsrx)
- [ ] Three screenshots: tiled terminals, agent driving desktop via MCP, theme swap
- [ ] README.org with install command front-and-center
- [ ] "Why miozu" doc comparing to Hyprland/Sway (tiling without AI), GNOME
      (AI without tiling), ChromeOS (AI + nothing for developers)
- [ ] One blog post on `teru.sh/blog/` announcing miozu.

## Open decisions

1. **Do we sell Miozu Pro now or wait for traction?** Recommend: wait. Ship
   free + great first.
2. **Does `teruwm` ever run on X11?** Current code is Wayland-only. Leave
   XMonad as the X11 story; don't bifurcate.
3. **Gnosis MCP bundled or opt-in?** Recommend: bundled, behind a
   `gnosis.enabled = true` flag in `ai.conf`, default true for demo impact.
4. **Should `miozu` the brand show `selify.ai` anywhere?** No — keep stealth
   separation. Personal hiring-signal site `nicgl.com` can link everything.

## Next actions

- Land keyboard/locale fix (Task 2).
- Draft new `README.org` against this positioning.
- Record first hsrx demo GIF for `miozu.com`.
- Write `.miozu/docs/CONFIG.md` describing the `~/.config/miozu/` layout.
