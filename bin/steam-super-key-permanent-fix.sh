#!/bin/bash

echo "=== Steam Super Key Permanent Fix ==="
echo ""
echo "This script implements multiple solutions for Steam grabbing the Super key"
echo ""

# Solution 1: Set SDL environment variable globally
echo "1. Setting SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0 globally..."
if ! grep -q "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS" ~/.profile 2>/dev/null; then
    echo "export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0" >> ~/.profile
    echo "   Added to ~/.profile"
else
    echo "   Already set in ~/.profile"
fi

# Solution 2: Create Steam wrapper script
echo ""
echo "2. Creating Steam wrapper script..."
cat > ~/.miozu/bin/steam-wrapper.sh << 'EOF'
#!/bin/bash
# Steam wrapper that prevents Super key grabbing
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOST=0

# Kill any existing Steam input processes
pkill -f "steamwebhelper" 2>/dev/null || true

# Launch Steam with environment variables
exec /usr/bin/steam "$@"
EOF
chmod +x ~/.miozu/bin/steam-wrapper.sh
echo "   Created ~/.miozu/bin/steam-wrapper.sh"

# Solution 3: Create desktop file override
echo ""
echo "3. Creating Steam desktop file override..."
mkdir -p ~/.local/share/applications
cat > ~/.local/share/applications/steam.desktop << 'EOF'
[Desktop Entry]
Name=Steam (Fixed)
Comment=Application for managing and playing games on Steam
Exec=env SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0 /usr/bin/steam %U
Icon=steam
Terminal=false
Type=Application
Categories=Network;FileTransfer;Game;
MimeType=x-scheme-handler/steam;x-scheme-handler/steamlink;
Actions=Store;Community;Library;Servers;Screenshots;News;Settings;BigPicture;Friends;
EOF
echo "   Created ~/.local/share/applications/steam.desktop"

# Solution 4: Add to XMonad startup
echo ""
echo "4. Checking XMonad startup hook..."
if grep -q "SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS" ~/.config/xmonad/lib/Hooks.hs 2>/dev/null; then
    echo "   Already configured in XMonad"
else
    echo "   Add this to your XMonad startup hook:"
    echo '   spawn "export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0"'
fi

# Solution 5: Immediate fix
echo ""
echo "5. Applying immediate fix..."
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0
export SDL_VIDEO_MINIMIZE_ON_FOCUS_LOST=0
xdotool key XF86Ungrab 2>/dev/null || true
xmodmap -e "clear mod4" 2>/dev/null
xmodmap -e "add mod4 = Super_L Super_R" 2>/dev/null

echo ""
echo "=== IMPORTANT MANUAL STEPS ==="
echo ""
echo "1. IN STEAM:"
echo "   - Go to Steam → Settings → Controller"
echo "   - Click 'Desktop Configuration'"
echo "   - Disable 'Guide Button Focuses Steam'"
echo "   - OR disable Desktop Configuration entirely"
echo ""
echo "2. FOR INDIVIDUAL GAMES:"
echo "   - Right-click game → Properties → Launch Options"
echo "   - Add: SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS=0 %command%"
echo ""
echo "3. RESTART STEAM:"
echo "   - Close Steam completely"
echo "   - Relaunch using: ~/.miozu/bin/steam-wrapper.sh"
echo "   - Or just type 'steam' if using the desktop file"
echo ""
echo "Your Super key should work now!"