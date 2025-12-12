#!/bin/bash

# Toggle bottom xmobar visibility
BOTTOM_XMOBAR_PID=$(pgrep -f "xmobar.*xmobar-bottom.hs")

if [ -n "$BOTTOM_XMOBAR_PID" ]; then
    # If running, kill it
    kill "$BOTTOM_XMOBAR_PID"
else
    # If not running, start it
    xmobar ~/.config/xmonad/xmobar/xmobar-bottom.hs &
fi