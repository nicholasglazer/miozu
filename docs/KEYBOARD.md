# Miozu Keyboard Layout — Architecture

How keyboard layout (Dvorak + Ukrainian, Alt+Shift toggle, Caps=Esc) is
applied across every context miozu runs in. Written during the X11 → Wayland
transition, so both paths are documented.

## Precedence (Wayland)

Four layers, highest priority first. Any layer leaving a field unset defers
to the layer below.

1. **teruwm `[keyboard]` section** in `~/.config/teruwm/config` — compositor
   config is the source of truth (Sway / Hyprland / niri / Wayfire pattern).
2. **User override** `~/.config/environment.d/20-user-keyboard.conf` —
   installer writes this for non-Dvorak users. `XKB_DEFAULT_*` env vars.
3. **Miozu default** `~/.config/environment.d/10-miozu-keyboard.conf` —
   symlinked from the repo; Dvorak + UA + grp:alt_shift_toggle + caps:escape.
4. **libxkbcommon built-in default** — `us` QWERTY, no options.

systemd's user manager loads environment.d files in alphabetical order, so
`20-*` overrides `10-*` for free — no custom mechanism.

## Context matrix

| Context | Source file | Mechanism |
|---|---|---|
| TTY console (tty1–tty6) | `/etc/vconsole.conf` | kernel keymap via `systemd-vconsole-setup.service` |
| X11 / XMonad | `/etc/X11/xorg.conf.d/00-keyboard.conf` | Xorg auto-loads at server start |
| teruwm (compositor) | `~/.config/teruwm/config` `[keyboard]` → env vars → defaults | built-in three-layer fallback in `Server.setupKeyboard` |
| Sway / Hyprland / GNOME / etc. | `~/.config/environment.d/*.conf` → `XKB_DEFAULT_*` | systemd user manager injects env into graphical-session.target |
| Interactive fish shells | `.config/fish/config.fish` | sources the environment.d file |
| TTY-launched teruwm | `bin/run-teruwm.sh` | sources the environment.d file |
| WSL | `bin/lib/wsl.sh` | `setxkbmap` inline |

**Critical distinction:** XKB (setxkbmap, `XKB_DEFAULT_*`, xkbcommon) is
userspace, X11/Wayland only. It does **nothing** to a bare TTY. TTYs use the
kernel keymap loaded from `/etc/vconsole.conf`.

## The canonical env file

`.config/environment.d/10-miozu-keyboard.conf`:

```ini
XKB_DEFAULT_LAYOUT=us,ua
XKB_DEFAULT_VARIANT=dvorak,
XKB_DEFAULT_OPTIONS=grp:alt_shift_toggle,caps:escape
```

Installed to `~/.config/environment.d/10-miozu-keyboard.conf` as a symlink by
`bin/lib/keyboard.sh::setup_keyboard_wayland()` (runs during `./bin/install.sh`).

### Who reads it automatically

- **systemd user manager** — on `default.target` startup, env vars from
  `~/.config/environment.d/*.conf` are injected into every user unit. Any
  Wayland compositor launched via `systemctl --user start X.service` or a
  graphical-session.target inherits them. Reference:
  [`environment.d(5)`](https://www.freedesktop.org/software/systemd/man/environment.d.html).

### Who sources it explicitly

- `.config/fish/config.fish` — parses the file and `set -x`'s each entry.
  Required because interactive TTY shells aren't necessarily children of the
  systemd user session's graphical target.
- `bin/run-teruwm.sh` — `set -a; . "$KB_ENV"; set +a`. Required when teruwm
  is launched from a bare TTY (no systemd-logind user manager on the path).

## teruwm `[keyboard]` section (highest priority)

teruwm reads `~/.config/teruwm/config` and builds an `xkb_rule_names` struct
from this section. Unset fields pass NULL through to libxkbcommon, which
consults `XKB_DEFAULT_*` env vars — hence the layered fallback.

```ini
[keyboard]
xkb_layout = us,ua
xkb_variant = dvorak,
xkb_options = grp:alt_shift_toggle,caps:escape
# optional:
xkb_model = pc105
xkb_rules = evdev
```

Naming is Sway-compatible (`xkb_layout` etc.). Hyprland-style names
(`kb_layout`) and bare names (`layout`) are also accepted for compatibility.

**Hot reload**: `Mod+Shift+R` re-reads the config and re-applies the keymap
to the currently-active keyboard without reconnecting the device. Multi-
keyboard setups need to reconnect secondary devices for now (tracked — see
Server.zig `reloadWmConfig`).

## Non-Dvorak users (the installer flow)

`./bin/install.sh` prompts for Dvorak or QWERTY. Behind the scenes:

- **Dvorak** → `setup_keyboard_wayland` symlinks the repo default; any stale
  `20-user-keyboard.conf` is removed. System-level X11/vconsole set to Dvorak
  via `localectl`.
- **QWERTY** → `setup_keyboard_wayland` still installs the repo default
  (other programs that want Dvorak don't care — they stay at layer 2), then
  `install_wayland_override` writes `20-user-keyboard.conf` with plain `us`
  and empty variant/options. System-level X11/vconsole set to `us`.

Users who want a different layout later can edit `20-user-keyboard.conf`
directly — no installer rerun needed.

## X11 (legacy, still active)

`/etc/X11/xorg.conf.d/00-keyboard.conf` is managed by `localectl` and Xorg
reads it on server start. The values match the Wayland canonical file.

Runtime re-apply (e.g., after a program does `XUngrabKeyboard` badly):

```sh
setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'
```

Scripts that do this:
- `bin/fix-steam-leader-grab.sh`
- `bin/force-ungrab-keys.sh`
- `bin/debug-leader-key.sh`
- xmobar `%kbd%` click action (`xmobar.hs`, `xmobar-top.hs`)

These are X11-only and not needed on Wayland (wlroots manages keyboard state
cleanly).

## TTY (kernel keymap)

`/etc/vconsole.conf`:

```ini
KEYMAP=dvorak
XKBLAYOUT=us
XKBMODEL=pc105
XKBVARIANT=dvorak
FONT=ter-v14n
```

Applied at boot by `systemd-vconsole-setup.service`. Ukrainian is **not**
available on the TTY — the kernel keymap is single-layout. That's a
limitation of the console, not a miozu decision.

### Troubleshooting

If a TTY shows QWERTY despite `KEYMAP=dvorak`:

```sh
sudo loadkeys dvorak                           # immediate, this boot only
sudo systemctl restart systemd-vconsole-setup  # re-apply from vconsole.conf
sudo localectl set-keymap dvorak               # canonical: writes + applies
```

If `keymap` is in `HOOKS` in `/etc/mkinitcpio.conf`, also rerun
`sudo mkinitcpio -P` after changing vconsole.conf so initramfs catches up.

## Design choices — why this layering

Researched reference compositors before committing:

| Compositor | Config format | Env vars as fallback? |
|---|---|---|
| Sway | `input ... xkb_layout` | Yes (via libxkbcommon default) |
| Hyprland | `input { kb_layout = }` | No (tracked as bug #4381) |
| niri | `input.keyboard.xkb` | No — uses systemd-localed D-Bus |
| Wayfire | `[input] xkb_layout` | Undocumented |
| river | `riverctl keyboard-layout` | Yes (libxkbcommon default) |
| labwc | Pure env vars in XML | N/A (env *is* the config) |

Miozu follows **Sway's pattern**: compositor config is authoritative when
set, env vars fall back when the compositor config is silent. This gives
three wins:

1. GTK/Qt/XWayland apps agree on layout (they read env vars).
2. Multi-compositor setups (Sway + teruwm + Hyprland) share a single source.
3. Per-user overrides use a native systemd mechanism, not a custom one.

niri's systemd-localed D-Bus fallback is more forward-looking for distro
integration (a package manager could set the layout centrally), but it's a
v2 nicety, not a blocker.

## Changing the layout

By priority:

```sh
# Everything (teruwm + GTK + XWayland):
$EDITOR ~/.config/environment.d/20-user-keyboard.conf   # or 10-miozu if personal fork
# Then log out and back in, or restart the compositor.

# teruwm only (wins over env vars):
$EDITOR ~/.config/teruwm/config      # [keyboard] section; Mod+Shift+R to reload

# X11 (XMonad):
sudo localectl set-x11-keymap us,ua pc105 dvorak, grp:alt_shift_toggle,caps:escape

# TTY console:
sudo localectl set-keymap dvorak
```

## Migration note (X11 → Wayland)

The X11 setup predates the Wayland one. Prior to 2026-04-13 the XKB triple
was hardcoded in four places (fish config, run-teruwm, environment.d,
bin/lib/keyboard.sh). The consolidation made `environment.d/10-miozu-keyboard.conf`
the single source for layers 2–3; fish and run-teruwm source it. teruwm's
`[keyboard]` section was added at the same time as a higher-priority layer
so the compositor reflects user intent even if env isn't inherited. X11
path is untouched because XMonad is still a daily driver.
