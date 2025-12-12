#!/bin/bash

echo "BDO-specific input fix starting..."

# Kill the background fix if running
pkill -f "aggressive-leader-fix.sh" 2>/dev/null

# Method 1: Try to modify Wine/Proton window properties
BDO_WINDOWS=$(xdotool search --name "Black Desert" 2>/dev/null || true)
for window in $BDO_WINDOWS; do
    # Remove override redirect which can cause grab issues
    xprop -id $window -remove _MOTIF_WM_HINTS 2>/dev/null || true
done

# Method 2: Set Wine to not grab keyboard
export WINEDEBUG=-all
export WINE_DISABLE_FULLSCREEN_HACK=1

# Method 3: Use wmctrl to manage window
if command -v wmctrl &> /dev/null; then
    wmctrl -r "Black Desert" -b remove,fullscreen 2>/dev/null || true
    wmctrl -r "Black Desert" -b add,maximized_vert,maximized_horz 2>/dev/null || true
fi

# Method 4: Continuous fix specifically for BDO
echo "Running continuous fix for BDO..."
while pgrep -f "Black Desert" > /dev/null; do
    # Ungrab
    xdotool key --clearmodifiers XF86Ungrab 2>/dev/null
    
    # Reset Super without touching other keys
    xmodmap -e "remove mod4 = Super_L Super_R" 2>/dev/null
    xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null
    
    # Brief pause
    sleep 0.5
done &

echo "BDO input fix active. Your leader key should keep working."
echo "This will run until BDO closes."