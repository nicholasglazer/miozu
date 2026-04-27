#!/bin/bash
# Launcher for teruwm from any TTY.
# Usage: ./run-teruwm.sh [debug|release|skip]
#
# Default is `debug`: always rebuilds, aborts if build breaks, so you
# never silently launch stale code. Zig is incremental — on no-change
# rebuild the cost is ~50ms. Use `skip` only when you deliberately want
# to run the last-built binary (bisect, debugging a launch issue).

set -euo pipefail

cd ~/code/workbench/foss/teru

# Auto-detect + fix GCC 15 .sframe CRT issue (no-op if not needed)
./tools/fix-crt.sh >/dev/null 2>&1 || true
LIBC_FLAG=$([ -f .cache/crt-fix-all/libc.txt ] && echo "--libc .cache/crt-fix-all/libc.txt" || echo "")

MODE="${1:-debug}"
case "$MODE" in
    release) zig build -Dcompositor -Doptimize=ReleaseSafe $LIBC_FLAG
             strip zig-out/bin/teruwm ;;
    debug)   zig build -Dcompositor $LIBC_FLAG ;;
    skip)    echo "run-teruwm: skip mode — running last-built binary" ;;
    *)       echo "usage: $0 [debug|release|skip]" >&2; exit 2 ;;
esac

# Show what we're about to run so there's no ambiguity about stale code.
commit=$(git rev-parse --short HEAD 2>/dev/null || echo unknown)
dirty=$(git diff --quiet 2>/dev/null && echo clean || echo dirty)
built=$(stat -c %y zig-out/bin/teruwm 2>/dev/null | cut -d. -f1 || echo missing)
echo "run-teruwm: $commit $dirty  built=$built"

# Keyboard layout fallback for TTY launch (env inheritance break, see
# docs/KEYBOARD.md). Canonical source: ~/.config/environment.d/10-miozu-keyboard.conf.
# Redundant with the [keyboard] section in ~/.config/teruwm/config, kept as
# belt-and-suspenders for GTK/Qt/XWayland clients teruwm spawns.
KB_ENV="$HOME/.config/environment.d/10-miozu-keyboard.conf"
[ -f "$KB_ENV" ] && { set -a; . "$KB_ENV"; set +a; }

# Clean stale Wayland locks from crashed sessions
rm -f "/run/user/$(id -u)/wayland-"*.lock 2>/dev/null || true

exec ./zig-out/bin/teruwm
