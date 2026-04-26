#!/bin/sh
# Set up the internal display for ThinkPad P1 Gen 7 (2560x1600 eDP).
# Retries xrandr a few times — the i915 CRTC isn't always ready immediately
# after X starts.

for i in 1 2 3 4 5; do
  xrandr --output eDP-1 --mode 2560x1600 --primary && exit 0
  sleep 1
done

# Last attempt with --auto as fallback
xrandr --output eDP-1 --auto --primary
