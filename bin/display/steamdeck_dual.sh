#!/bin/bash
# TODO make varibales like rate global . conf

set -euo pipefail

# Might help to figure out if "xrandr" is not helpful: "ls /sys/class/drm"
INTERNAL_NAME="eDP"

# Get the name of the connected external monitor (other than the internal display)
EXTERNAL_MONITOR=$(xrandr | grep -w "connected" | grep -v "$INTERNAL_NAME" | awk '{print $1}') || true
INTERNAL_MONITOR=$(xrandr | grep -w "connected" | grep "$INTERNAL_NAME" | awk '{print $1}')

# The i915 CRTC occasionally takes a few seconds after X start to
# accept mode configuration (race between X init and DRM master handoff).
# Retry xrandr if it fails.
set_mode() {
  local cmd=("$@")
  for i in 1 2 3 4 5; do
    "${cmd[@]}" && return 0
    sleep 1
  done
  return 1
}

if [ -n "$EXTERNAL_MONITOR" ]; then
  # External monitor connected: set it as primary, rotate internal to the right
  set_mode xrandr --output "$EXTERNAL_MONITOR" --mode 1920x1080 --primary --rate 60 --pos 0x0 \
           --output "$INTERNAL_MONITOR" --rotate right --pos 1921x0
else
  # No external monitor — just set internal to preferred mode
  set_mode xrandr --output "$INTERNAL_MONITOR" --auto --rotate normal --primary
fi
