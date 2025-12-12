#!/bin/bash
# Script to switch XMonad workspace by index
# Usage: xmonad-switch-workspace.sh <workspace_index>

workspace_index="$1"

if [ -z "$workspace_index" ]; then
    echo "Usage: $0 <workspace_index>"
    exit 1
fi

# Map workspace index to key combination
# Note: workspace 10 is mapped to 0 key
if [ "$workspace_index" = "10" ]; then
    xdotool key super+0
else
    xdotool key super+$workspace_index
fi