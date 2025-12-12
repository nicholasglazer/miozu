#!/bin/bash

echo "Forcing release of all key grabs..."

# Method 1: Use xdotool to send ungrab
xdotool key XF86Ungrab 2>/dev/null || true
xdotool key --clearmodifiers XF86Ungrab 2>/dev/null || true

# Method 2: Switch to a different window to break grabs
CURRENT_WINDOW=$(xdotool getactivewindow)
ROOT_WINDOW=$(xdotool search --maxdepth 0 --class "")
xdotool windowfocus $ROOT_WINDOW 2>/dev/null || true
sleep 0.1
xdotool windowfocus $CURRENT_WINDOW 2>/dev/null || true

# Method 3: Send escape key to break potential grabs
xdotool key Escape 2>/dev/null || true

# Method 4: Kill Steam input hook if it exists
pkill -f "steam.*input" 2>/dev/null || true
pkill -f "gameoverlayui" 2>/dev/null || true

# Method 5: Use Python X11 to force ungrab
python3 -c "
try:
    from Xlib import X, display
    d = display.Display()
    d.ungrab_keyboard(X.CurrentTime)
    d.ungrab_key(X.AnyKey, X.AnyModifier, d.screen().root)
    d.sync()
    print('Python: Ungrabbed keys successfully')
except Exception as e:
    print(f'Python ungrab failed: {e}')
" 2>/dev/null || echo "Python Xlib not available"

# Reset keyboard state
setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'

# Re-establish mod4 mapping
xmodmap -e "clear mod4"
xmodmap -e "keycode 133 = Super_L"
xmodmap -e "keycode 134 = Super_R"  
xmodmap -e "keycode 115 = Super_L"
xmodmap -e "keycode 116 = Super_R"
xmodmap -e "add mod4 = Super_L Super_R"

echo "Key ungrab attempted. Testing Super key..."
echo "Press Super key in next 3 seconds:"
timeout 3 xinput test-xi2 --root 2>/dev/null | grep -E "RawKeyPress|RawKeyRelease" | grep -A1 -B1 "133\|134\|115\|116" || echo "Still no Super key events!"

echo ""
echo "If still not working, try:"
echo "1. Alt+Tab away from any game/Steam window"
echo "2. Close Steam completely"
echo "3. Check if you have a gaming keyboard software running"