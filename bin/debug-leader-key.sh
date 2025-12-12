#!/bin/bash

echo "=== XMonad Leader Key Debug ==="
echo ""

echo "1. Current modifier mappings:"
xmodmap -pm | grep -E "mod[14]"
echo ""

echo "2. Super key keycodes:"
xmodmap -pke | grep -E "Super_[LR]"
echo ""

echo "3. Checking for key grabs:"
echo "Active window: $(xdotool getactivewindow getwindowname 2>/dev/null || echo "unknown")"
echo ""

echo "4. Testing if Super key physically works:"
echo "Press Super key now (5 second timeout)..."
timeout 5 xinput test-xi2 --root | grep -E "RawKeyPress|RawKeyRelease" | grep -A1 -B1 "133\|134\|115\|116" || echo "No Super key events detected!"
echo ""

echo "5. Attempting to fix..."
# Clear Steam's potential key grab
if pgrep -x steam > /dev/null; then
    echo "Steam is running - it might be grabbing keys"
    echo "Try: Alt+Tab to switch away from Steam window"
fi

# Force ungrab all keys
echo "Forcing ungrab of all keys..."
xdotool key XF86Ungrab || true

# Reset modifiers
echo "Resetting modifiers..."
setxkbmap -layout us -option
setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'

# Clear and re-add Super to mod4
xmodmap -e "clear mod4"
xmodmap -e "keycode 115 = Super_L"
xmodmap -e "keycode 116 = Super_R"
xmodmap -e "keycode 127 = Super_L"
xmodmap -e "keycode 134 = Super_R"
xmodmap -e "add mod4 = Super_L Super_R"

# Clear and re-add Alt to mod1
xmodmap -e "clear mod1"
xmodmap -e "keycode 64 = Alt_L"
xmodmap -e "keycode 113 = Alt_R"
xmodmap -e "add mod1 = Alt_L Alt_R"

echo ""
echo "6. Final state:"
xmodmap -pm | grep -E "mod[14]"
echo ""
echo "Try your leader key now!"