#!/bin/bash
# WSL2-specific functions for Miozu installer
# Handles detection, X11 configuration, and WSL-specific setup

# Detect if running in WSL
is_wsl() {
    if grep -qi microsoft /proc/version 2>/dev/null; then
        return 0
    fi
    return 1
}

# Detect WSL version (1 or 2)
get_wsl_version() {
    if [[ -f /proc/version ]]; then
        if grep -qi "microsoft.*WSL2" /proc/version 2>/dev/null; then
            echo "2"
        elif grep -qi microsoft /proc/version 2>/dev/null; then
            echo "1"
        fi
    fi
    echo "0"
}

# Get Windows host IP for X11 display (for VcXsrv)
get_windows_host_ip() {
    if [[ -f /etc/resolv.conf ]]; then
        grep nameserver /etc/resolv.conf | awk '{print $2}' | head -1
    fi
}

# Check if WSLg is available (Windows 11 with GUI support)
has_wslg() {
    [[ -d /mnt/wslg ]] && return 0
    return 1
}

# Configure X11 DISPLAY variable for WSL
# IMPORTANT: For window managers like XMonad, always use VcXsrv, not WSLg
configure_wsl_display() {
    print_header "Configuring X11 Display for WSL"

    local windows_ip
    windows_ip=$(get_windows_host_ip)

    if [[ -z "$windows_ip" ]]; then
        echo -e "${RED}Could not determine Windows host IP${NC}"
        return 1
    fi

    echo "Windows host IP: $windows_ip"

    if has_wslg; then
        echo -e "${YELLOW}WSLg detected but XMonad requires VcXsrv for full WM support${NC}"
        echo "Configuring for VcXsrv (you can still use WSLg for individual apps)"
    fi

    # Add to Fish config - always use VcXsrv IP for xmonad
    local fish_config="$HOME/.config/fish/config.fish"
    if [[ -f "$fish_config" ]]; then
        # Remove old WSL2 X11 Display config if present
        sed -i '/# WSL2 X11 Display/,/LIBGL_ALWAYS_SOFTWARE/d' "$fish_config" 2>/dev/null || true

        echo "Adding X11 DISPLAY configuration to Fish..."
        cat >> "$fish_config" << 'EOF'

# WSL2 X11 Display - Use VcXsrv for XMonad (window managers need full X server)
# For individual GUI apps, WSLg works fine with DISPLAY=:0
set -gx DISPLAY_VCXSRV (grep nameserver /etc/resolv.conf | awk '{print $2}'):0
set -gx LIBGL_ALWAYS_INDIRECT 1

# Add ~/.local/bin to PATH for start-xmonad script
if not contains $HOME/.local/bin $PATH
    set -gx PATH $HOME/.local/bin $PATH
end
EOF
        echo -e "${GREEN}Fish X11 config added${NC}"
    fi

    # Add to Bash config
    if [[ -f "$HOME/.bashrc" ]]; then
        # Remove old config
        sed -i '/# WSL2 X11 Display/,/LIBGL_ALWAYS/d' "$HOME/.bashrc" 2>/dev/null || true

        echo "Adding X11 DISPLAY configuration to Bash..."
        cat >> "$HOME/.bashrc" << 'EOF'

# WSL2 X11 Display - Use VcXsrv for XMonad (window managers need full X server)
# For individual GUI apps, WSLg works fine with DISPLAY=:0
export DISPLAY_VCXSRV=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
export LIBGL_ALWAYS_INDIRECT=1

# Add ~/.local/bin to PATH for start-xmonad script
export PATH="$HOME/.local/bin:$PATH"
EOF
        echo -e "${GREEN}Bash X11 config added${NC}"
    fi

    # Create .profile for login shells
    if [[ ! -f "$HOME/.profile" ]] || ! grep -q "\.local/bin" "$HOME/.profile"; then
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$HOME/.profile"
    fi

    echo -e "${GREEN}X11 display configured for VcXsrv: ${windows_ip}:0${NC}"
}

# Create xinitrc for WSL (used with startx if needed)
create_wsl_xinitrc() {
    print_header "Creating WSL X Startup Script"

    cat > "$HOME/.xinitrc" << 'EOF'
#!/bin/bash
# WSL2 XMonad startup script

# Start dbus session (required for xmobar and notifications)
if command -v dbus-launch &>/dev/null; then
    eval $(dbus-launch --sh-syntax)
    export DBUS_SESSION_BUS_ADDRESS
fi

# Set cursor
xsetroot -cursor_name left_ptr 2>/dev/null

# Set background color
xsetroot -solid "#1a1a2e" 2>/dev/null

# Start notification daemon
if command -v dunst &>/dev/null; then
    dunst &
fi

# Start XMonad
exec xmonad
EOF
    chmod +x "$HOME/.xinitrc"
    echo -e "${GREEN}Created ~/.xinitrc${NC}"
}

# Check if mirrored networking mode is enabled (Windows 11)
is_mirrored_mode() {
    # In mirrored mode, localhost:6000 should be reachable when VcXsrv is running
    # Also check if we can reach the Windows host via localhost
    if timeout 1 bash -c "echo > /dev/tcp/localhost/6000" 2>/dev/null; then
        return 0
    fi
    return 1
}

# Find working DISPLAY for X11
find_working_display() {
    local windows_ip
    windows_ip=$(get_windows_host_ip)

    # Try displays in order of preference
    local displays=()

    # Check mirrored mode first (Windows 11)
    if is_mirrored_mode; then
        displays+=("localhost:0")
    fi

    # Traditional NAT mode - use Windows host IP
    if [[ -n "$windows_ip" ]]; then
        displays+=("${windows_ip}:0")
    fi

    # Fallback
    displays+=("127.0.0.1:0" ":0")

    for disp in "${displays[@]}"; do
        export DISPLAY="$disp"
        if timeout 2 xset q &>/dev/null 2>&1; then
            echo "$disp"
            return 0
        fi
    done

    # Return the most likely one even if not working (for error messages)
    if [[ -n "$windows_ip" ]]; then
        echo "${windows_ip}:0"
    else
        echo "localhost:0"
    fi
    return 1
}

# Create convenient start script for XMonad on WSL
create_wsl_start_script() {
    print_header "Creating WSL XMonad Start Script"

    mkdir -p "$HOME/.local/bin"

    cat > "$HOME/.local/bin/start-xmonad" << 'EOF'
#!/bin/bash
# Start XMonad on WSL2 with VcXsrv
# NOTE: XMonad (and other WMs) REQUIRE VcXsrv - WSLg cannot run window managers
#       because it has its own compositor that manages windows.
#
# This script auto-detects mirrored networking mode (Windows 11) vs NAT mode
# It also tries display :1 if :0 is taken by WSLg

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

# Get Windows host IP for NAT mode
WINDOWS_IP=$(grep nameserver /etc/resolv.conf 2>/dev/null | awk '{print $2}' | head -1)

# Function to test X connection
test_display() {
    local disp="$1"
    DISPLAY="$disp" timeout 2 xset q &>/dev/null 2>&1
}

# Find working display
find_display() {
    # If user specified display, use it
    if [[ -n "$1" ]]; then
        echo "$1"
        return 0
    fi

    # Try displays in order - :1 first (VcXsrv when WSLg uses :0)
    # Then :0 for systems without WSLg
    local displays=(
        "localhost:1"      # Mirrored mode, display :1 (WSLg uses :0)
        "localhost:0"      # Mirrored mode, display :0
        "127.0.0.1:1"      # Mirrored mode alternative
        "127.0.0.1:0"      # Mirrored mode alternative
    )

    # Add NAT mode displays if we have Windows IP
    if [[ -n "$WINDOWS_IP" ]]; then
        displays+=("${WINDOWS_IP}:1" "${WINDOWS_IP}:0")
    fi

    for disp in "${displays[@]}"; do
        if test_display "$disp"; then
            echo "$disp"
            return 0
        fi
    done

    # Nothing worked
    return 1
}

echo -e "${CYAN}Starting XMonad on WSL2...${NC}"

# Find working display
if DISPLAY=$(find_display "$1"); then
    export DISPLAY
    echo -e "Using DISPLAY=${GREEN}$DISPLAY${NC}"
else
    echo ""
    echo -e "${RED}ERROR: Cannot connect to X server${NC}"
    echo ""
    echo -e "${YELLOW}Tested displays:${NC}"
    echo "  - localhost:1, localhost:0 (mirrored mode)"
    echo "  - ${WINDOWS_IP}:1, ${WINDOWS_IP}:0 (NAT mode)"
    echo ""
    echo -e "${YELLOW}Make sure VcXsrv is running on Windows:${NC}"
    echo "  Run in Command Prompt (NOT PowerShell):"
    echo '  "C:\Program Files\VcXsrv\vcxsrv.exe" :1 -fullscreen -clipboard -wgl -ac'
    echo ""
    echo "  NOTE: Use :1 because WSLg uses :0"
    echo ""
    echo -e "${YELLOW}If VcXsrv is running, fix the firewall:${NC}"
    echo "  1. Copy firewall fix to Windows:"
    echo "     cp ~/.miozu/docs/windows/fix-wsl-firewall.bat /mnt/c/Users/YOUR_USER/Desktop/"
    echo "  2. Double-click fix-wsl-firewall.bat on Windows (runs as admin)"
    echo "  3. Restart VcXsrv and try again"
    echo ""
    echo -e "${YELLOW}Run diagnostic:${NC} ~/.miozu/bin/fix-wsl-x11.sh"
    exit 1
fi

export LIBGL_ALWAYS_INDIRECT=1

# Start dbus if not running
if [[ -z "$DBUS_SESSION_BUS_ADDRESS" ]]; then
    eval $(dbus-launch --sh-syntax)
    export DBUS_SESSION_BUS_ADDRESS
fi

echo -e "${GREEN}X server connection OK!${NC}"

# Configure keyboard: us,ua layouts with dvorak variant
# Alt+Shift to toggle language, Caps Lock as Escape (same as native Miozu)
echo "Configuring keyboard..."
echo "  Layout: us,ua (dvorak)"
echo "  Alt+Shift: toggle language"
echo "  Caps Lock: Escape"
setxkbmap -layout "us,ua" -variant "dvorak," -option "grp:alt_shift_toggle,caps:escape" 2>/dev/null || true

# Set cursor properly - try multiple methods
echo "Setting cursor..."
xsetroot -cursor_name left_ptr 2>/dev/null || true
# Set X resources for cursor
if command -v xrdb &>/dev/null; then
    echo "Xcursor.theme: default" | xrdb -merge 2>/dev/null || true
fi

# Set background color
xsetroot -solid "#1a1a2e" 2>/dev/null || true

# Start dunst for notifications
if command -v dunst &>/dev/null; then
    pkill dunst 2>/dev/null || true
    dunst &
fi

echo ""
echo -e "${YELLOW}NOTE: Windows/Super key captured by Windows by default.${NC}"
echo -e "${YELLOW}Run xmonad-keys.ahk on Windows to fix. Alt+Tab works with Windows.${NC}"
echo ""
echo "Starting XMonad..."

# Start XMonad (use exec to replace shell process)
exec xmonad
EOF
    chmod +x "$HOME/.local/bin/start-xmonad"
    echo -e "${GREEN}Created ~/.local/bin/start-xmonad${NC}"

    # Also create a simple alias script for those who don't have PATH set
    cat > "$HOME/start-xmonad.sh" << 'EOF'
#!/bin/bash
# Convenience script - runs ~/.local/bin/start-xmonad
exec "$HOME/.local/bin/start-xmonad" "$@"
EOF
    chmod +x "$HOME/start-xmonad.sh"
    echo -e "${GREEN}Created ~/start-xmonad.sh (backup if PATH not set)${NC}"
}

# Setup systemd for WSL (if enabled)
setup_wsl_systemd() {
    print_header "Checking WSL Systemd Support"

    local wsl_conf="/etc/wsl.conf"

    # Check if systemd is already enabled
    if systemctl --version &>/dev/null 2>&1; then
        echo -e "${GREEN}Systemd is available in this WSL instance${NC}"
        return 0
    fi

    echo "Systemd is not enabled in WSL"
    echo ""
    echo "To enable systemd in WSL2, add to /etc/wsl.conf:"
    echo ""
    echo "[boot]"
    echo "systemd=true"
    echo ""
    echo -n "Enable systemd now? [y/N]: "
    read -r reply < /dev/tty

    if [[ $reply =~ ^[Yy]$ ]]; then
        sudo tee "$wsl_conf" > /dev/null << 'EOF'
[boot]
systemd=true

[interop]
enabled=true
appendWindowsPath=true
EOF
        echo -e "${GREEN}Systemd enabled in wsl.conf${NC}"
        echo -e "${YELLOW}You must restart WSL for this to take effect:${NC}"
        echo "  In PowerShell: wsl --shutdown"
        echo "  Then reopen your WSL terminal"
        return 2  # Return code indicating restart needed
    else
        echo "Skipping systemd setup"
        echo "Some features (emacs daemon, user services) won't work without systemd"
        return 1
    fi
}

# Print WSL-specific instructions
print_wsl_instructions() {
    print_header "WSL2 Setup Complete!"

    local windows_ip
    windows_ip=$(get_windows_host_ip)

    echo -e "${GREEN}Miozu is installed for WSL2!${NC}"
    echo ""

    echo -e "${YELLOW}IMPORTANT: XMonad requires VcXsrv (not WSLg)${NC}"
    echo "WSLg cannot run window managers - it has its own compositor."
    echo ""
    echo "To start XMonad:"
    echo "================"
    echo ""
    echo "1. On Windows: Double-click 'xmonad-fullscreen.xlaunch'"
    echo "   (Copy it first: cp ~/.miozu/docs/windows/xmonad-fullscreen.xlaunch /mnt/c/Users/YOUR_USER/Desktop/)"
    echo ""
    echo "2. You'll see a black fullscreen window - this is correct!"
    echo ""
    echo "3. Alt-Tab back to WSL terminal and run:"
    echo "   ~/start-xmonad.sh"
    echo "   (or: ~/.local/bin/start-xmonad if PATH is set)"
    echo ""
    echo "4. Alt-Tab to VcXsrv window - XMonad should now be running!"
    echo "   Press Mod+Shift+Enter to open a terminal"
    echo ""

    if ! command -v vcxsrv &>/dev/null 2>&1; then
        echo "Install VcXsrv on Windows:"
        echo "  winget install marha.VcXsrv"
        echo ""
    fi

    echo "Firewall (run in PowerShell as Admin if needed):"
    echo '  New-NetFirewallRule -DisplayName "VcXsrv" -Direction Inbound -Program "C:\Program Files\VcXsrv\vcxsrv.exe" -Action Allow'
    echo ""

    echo "Key differences from native Linux:"
    echo "-----------------------------------"
    echo "- Must use VcXsrv for XMonad (WSLg only for individual apps)"
    echo "- No hardware audio (use Windows audio)"
    echo "- No bluetooth"
    echo "- Super/Win key may conflict with Windows"
    echo ""
    echo "Documentation: ~/.miozu/docs/WSL2_XMONAD_SETUP.md"
    echo ""

    if ! systemctl --version &>/dev/null 2>&1; then
        echo -e "${YELLOW}Note: Systemd is not enabled. Some services won't work.${NC}"
        echo "Enable with: sudo nano /etc/wsl.conf, add [boot] systemd=true"
        echo ""
    fi
}

# Check WSL prerequisites
check_wsl_prerequisites() {
    print_header "Checking WSL Prerequisites"

    local errors=0

    # Check if running in WSL
    if ! is_wsl; then
        echo -e "${RED}Not running in WSL. Use ./bin/install.sh for native Linux.${NC}"
        exit 1
    fi
    echo -e "${GREEN}WSL environment detected${NC}"

    # Check WSL version
    local wsl_ver
    wsl_ver=$(get_wsl_version)
    if [[ "$wsl_ver" == "2" ]]; then
        echo -e "${GREEN}WSL2 detected (recommended)${NC}"
    elif [[ "$wsl_ver" == "1" ]]; then
        echo -e "${YELLOW}WSL1 detected - WSL2 is recommended for better X11 support${NC}"
    fi

    # Check for basic tools
    for cmd in git curl sudo; do
        if command -v "$cmd" &>/dev/null; then
            echo -e "${GREEN}$cmd available${NC}"
        else
            echo -e "${RED}$cmd not found - installing...${NC}"
            sudo pacman -S --noconfirm --needed "$cmd" || ((errors++))
        fi
    done

    # Check WSLg
    if has_wslg; then
        echo -e "${YELLOW}WSLg available (but XMonad needs VcXsrv for full WM)${NC}"
    else
        echo "WSLg not detected - will use VcXsrv for X11"
    fi

    return $errors
}
