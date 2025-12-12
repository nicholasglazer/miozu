#!/bin/bash

echo "Testing X11 key registration - Press Super/Windows key, then Ctrl+C to stop"
echo "If you see KeyPress/KeyRelease events, X11 is receiving the key."
echo "If not, something is grabbing it."
echo ""

# Run xev and filter for Super key events
xev -event keyboard | grep -A2 -B2 "Super_[LR]\|keycode 115\|keycode 116\|keycode 133\|keycode 134"