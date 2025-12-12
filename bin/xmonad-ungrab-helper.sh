#!/bin/bash

# This uses XMonad's approach to ungrabbing
# Based on XMonad.Util.Ungrab

echo "Using XMonad ungrab approach..."

# First, tell XMonad to ungrab
xdotool key --window root XF86Ungrab

# Then sync with X server
xdotool sync

# Now reset the Super key
xmodmap -e "clear mod4"
xmodmap -e "keycode 133 = Super_L NoSymbol Super_L"
xmodmap -e "keycode 134 = Super_R NoSymbol Super_R"
xmodmap -e "add mod4 = Super_L Super_R"

echo "Done. Try your leader key now."