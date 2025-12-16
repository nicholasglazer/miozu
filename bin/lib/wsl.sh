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

# Create convenient start script for XMonad on WSL
create_wsl_start_script() {
    print_header "Creating WSL XMonad Start Script"

    mkdir -p "$HOME/.local/bin"

    cat > "$HOME/.local/bin/start-xmonad" << 'EOF'
#!/bin/bash
# Start XMonad on WSL2 with VcXsrv
# NOTE: XMonad (and other WMs) REQUIRE VcXsrv - WSLg cannot run window managers
#       because it has its own compositor that manages windows.

# Always use VcXsrv for XMonad (not WSLg)
VCXSRV_DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0

# Check if user wants to override
if [[ -n "$1" ]]; then
    export DISPLAY="$1"
else
    export DISPLAY="$VCXSRV_DISPLAY"
fi

export LIBGL_ALWAYS_INDIRECT=1

echo "Using DISPLAY=$DISPLAY"

# Start dbus if not running
if [[ -z "$DBUS_SESSION_BUS_ADDRESS" ]]; then
    eval $(dbus-launch --sh-syntax)
    export DBUS_SESSION_BUS_ADDRESS
fi

# Test X connection
echo "Testing X server connection..."
if ! timeout 3 xset q &>/dev/null 2>&1; then
    echo ""
    echo "ERROR: Cannot connect to X server at $DISPLAY"
    echo ""
    echo "Make sure VcXsrv is running on Windows:"
    echo "  1. Double-click 'xmonad-fullscreen.xlaunch' on your Windows Desktop"
    echo "  2. You should see a black/gray fullscreen window"
    echo "  3. Alt-Tab back to this terminal and run 'start-xmonad' again"
    echo ""
    echo "If VcXsrv is running but still fails:"
    echo "  - Check Windows Firewall allows VcXsrv"
    echo "  - Try: export DISPLAY=$VCXSRV_DISPLAY && xterm"
    echo ""
    exit 1
fi

echo "X server connection OK!"
echo "Starting XMonad..."

# Set cursor and background
xsetroot -cursor_name left_ptr 2>/dev/null
xsetroot -solid "#1a1a2e" 2>/dev/null

# Start dunst for notifications
if command -v dunst &>/dev/null; then
    pkill dunst 2>/dev/null
    dunst &
fi

# Start XMonad
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
