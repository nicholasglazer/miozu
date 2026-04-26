#!/bin/bash
# Quick launcher for teruwm — run from any TTY
# Usage: ./run-teruwm.sh [release|debug]

cd ~/code/foss/teru || exit 1

# Build if requested or binary is older than source
MODE=${1:-skip}
if [ "$MODE" = "release" ]; then
    echo "Building release..."
    zig build -Dcompositor=true -Doptimize=ReleaseSafe && strip zig-out/bin/teruwm
elif [ "$MODE" = "debug" ]; then
    echo "Building debug..."
    zig build -Dcompositor=true
fi

# Keyboard layout fallback for TTY launch.
# Canonical source: ~/.config/environment.d/10-miozu-keyboard.conf
# (systemd user session reads it on graphical-session.target). When teruwm
# is launched directly from a TTY without going through systemd-logind's
# user manager, those vars aren't inherited — source the same file so
# behaviour is identical and there's no duplicated config to drift.
KB_ENV="$HOME/.config/environment.d/10-miozu-keyboard.conf"
if [ -f "$KB_ENV" ]; then
    set -a
    . "$KB_ENV"
    set +a
fi

# Clean stale Wayland locks
rm -f /run/user/$(id -u)/wayland-*.lock 2>/dev/null

# Run
exec ./zig-out/bin/teruwm
