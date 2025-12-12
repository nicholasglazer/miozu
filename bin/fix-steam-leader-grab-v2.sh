#!/bin/bash

echo "Aggressive Steam Super key fix v2..."

# Step 1: Kill all Steam input-related processes
echo "1. Killing Steam input processes..."
pkill -f "steamwebhelper" 2>/dev/null || true
pkill -f "gameoverlayui" 2>/dev/null || true
sleep 0.5

# Step 2: Force ungrab without resetting entire keyboard
echo "2. Force ungrabbing keys..."
xdotool key XF86Ungrab 2>/dev/null || true
xdotool keyup Super_L Super_R 2>/dev/null || true

# Step 3: Only fix mod4, don't touch the rest
echo "3. Fixing ONLY mod4 mapping..."
xmodmap -e "clear mod4" 2>/dev/null
xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null

# Step 4: Minimize Steam window to prevent re-grab
echo "4. Minimizing Steam..."
STEAM_WINDOWS=$(xdotool search --class "Steam" 2>/dev/null || true)
for window in $STEAM_WINDOWS; do
    xdotool windowminimize $window 2>/dev/null || true
done

# Step 5: Focus on a different window to break Steam's focus
echo "5. Switching window focus..."
xdotool windowfocus $(xdotool search --class "XMonad" | head -1) 2>/dev/null || \
xdotool windowfocus $(xdotool getwindowfocus) 2>/dev/null || true

echo ""
echo "Done! Leader key should work now."
echo ""
echo "If it stops again in 2 seconds, Steam is auto-grabbing."
echo "Try: Alt+Tab to switch windows, or run the auto-fix script."