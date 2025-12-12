#!/bin/bash

echo "Fixing Steam's Super key grab issue..."
echo ""

# Method 1: Disable Steam Input Desktop Configuration
echo "1. Disabling Steam Input Desktop Configuration..."
# This prevents Steam from grabbing system keys when not in a game
pkill -f "steam.*desktop.*config" 2>/dev/null || true

# Method 2: Kill gameoverlayui if present
echo "2. Checking for Steam overlay processes..."
if pgrep -f "gameoverlayui" > /dev/null; then
    echo "   Killing Steam overlay UI..."
    pkill -f "gameoverlayui"
fi

# Method 3: Set Steam to not grab keys in desktop mode
echo "3. Forcing Steam to release desktop control..."
# Send Steam to background to release input focus
if pgrep -x "steam" > /dev/null; then
    STEAM_WINDOW=$(xdotool search --name "Steam" | head -1)
    if [ -n "$STEAM_WINDOW" ]; then
        xdotool windowminimize $STEAM_WINDOW 2>/dev/null || true
    fi
fi

# Method 4: Force ungrab and reset keys
echo "4. Resetting key mappings..."
xdotool key XF86Ungrab 2>/dev/null || true
xdotool key --clearmodifiers XF86Ungrab 2>/dev/null || true

# Reset keyboard
setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'

# Fix mod4
xmodmap -e "clear mod4"
xmodmap -e "keycode 133 = Super_L"
xmodmap -e "keycode 134 = Super_R"
xmodmap -e "add mod4 = Super_L Super_R"

echo ""
echo "Done! Your leader key should work now."
echo ""
echo "To prevent this permanently:"
echo "1. In Steam: Settings -> Controller -> Desktop Configuration"
echo "2. Disable 'Guide Button Focuses Steam'"
echo "3. Or disable Desktop Configuration entirely"
echo ""
echo "Alternative: Add to Steam launch options:"
echo "  SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0"