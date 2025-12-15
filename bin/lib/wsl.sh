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

# Get Windows host IP for X11 display
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
configure_wsl_display() {
    print_header "Configuring X11 Display for WSL"

    local windows_ip
    windows_ip=$(get_windows_host_ip)

    if has_wslg; then
        echo "WSLg detected - using native Wayland/X11 support"
        echo "DISPLAY is automatically set by WSLg"
        return 0
    fi

    if [[ -z "$windows_ip" ]]; then
        echo -e "${RED}Could not determine Windows host IP${NC}"
        return 1
    fi

    echo "Windows host IP: $windows_ip"

    # Add to Fish config if not present
    local fish_config="$HOME/.config/fish/config.fish"
    if [[ -f "$fish_config" ]]; then
        if ! grep -q "WSL2 X11 Display" "$fish_config"; then
            echo "Adding X11 DISPLAY configuration to Fish..."
            cat >> "$fish_config" << 'EOF'

# WSL2 X11 Display for VcXsrv
if test -f /etc/resolv.conf
    set -gx DISPLAY (grep nameserver /etc/resolv.conf | awk '{print $2}'):0
end
set -gx LIBGL_ALWAYS_SOFTWARE 1
EOF
            echo -e "${GREEN}Fish X11 config added${NC}"
        else
            echo "Fish X11 config already present"
        fi
    fi

    # Add to Bash config as fallback
    if [[ -f "$HOME/.bashrc" ]]; then
        if ! grep -q "WSL2 X11 Display" "$HOME/.bashrc"; then
            echo "Adding X11 DISPLAY configuration to Bash..."
            cat >> "$HOME/.bashrc" << 'EOF'

# WSL2 X11 Display for VcXsrv
export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
export LIBGL_ALWAYS_SOFTWARE=1
EOF
            echo -e "${GREEN}Bash X11 config added${NC}"
        fi
    fi

    # Set for current session
    export DISPLAY="${windows_ip}:0"
    export LIBGL_ALWAYS_SOFTWARE=1

    echo -e "${GREEN}X11 display configured: $DISPLAY${NC}"
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
# Start XMonad on WSL2 with VcXsrv or WSLg

# Set DISPLAY if not set
if [[ -z "$DISPLAY" ]]; then
    if [[ -d /mnt/wslg ]]; then
        # WSLg sets DISPLAY automatically
        export DISPLAY=:0
    else
        # VcXsrv - get Windows host IP
        export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
    fi
fi

export LIBGL_ALWAYS_SOFTWARE=1

# Start dbus if not running
if [[ -z "$DBUS_SESSION_BUS_ADDRESS" ]]; then
    eval $(dbus-launch --sh-syntax)
    export DBUS_SESSION_BUS_ADDRESS
fi

# Test X connection
if ! xset q &>/dev/null 2>&1; then
    echo "Error: Cannot connect to X server at $DISPLAY"
    echo ""
    echo "For VcXsrv: Start VcXsrv on Windows first (xmonad-fullscreen.xlaunch)"
    echo "For WSLg: Ensure WSLg is enabled in Windows 11"
    exit 1
fi

echo "Starting XMonad on display $DISPLAY..."
exec xmonad
EOF
    chmod +x "$HOME/.local/bin/start-xmonad"
    echo -e "${GREEN}Created ~/.local/bin/start-xmonad${NC}"
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

    if has_wslg; then
        echo "WSLg detected - you can run GUI apps natively!"
        echo ""
        echo "To start XMonad:"
        echo "  start-xmonad"
        echo ""
    else
        echo "Next steps on Windows:"
        echo "------------------------"
        echo "1. Install VcXsrv:"
        echo "   winget install marha.VcXsrv"
        echo ""
        echo "2. Copy launcher to Desktop:"
        echo "   cp ~/.miozu/docs/windows/xmonad-fullscreen.xlaunch /mnt/c/Users/YOUR_USER/Desktop/"
        echo ""
        echo "3. Add firewall rule (PowerShell as Admin):"
        echo '   New-NetFirewallRule -DisplayName "VcXsrv" -Direction Inbound -Program "C:\Program Files\VcXsrv\vcxsrv.exe" -Action Allow'
        echo ""
        echo "To start XMonad:"
        echo "----------------"
        echo "1. Double-click xmonad-fullscreen.xlaunch on Windows Desktop"
        echo "2. In WSL: start-xmonad"
        echo ""
    fi

    echo "Key differences from native Linux:"
    echo "-----------------------------------"
    echo "- No hardware audio (use Windows audio)"
    echo "- No bluetooth"
    echo "- No hardware brightness control"
    echo "- Super/Win key may conflict with Windows (see docs for fix)"
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

    # Check WSLg or ability to connect to X
    if has_wslg; then
        echo -e "${GREEN}WSLg available (native GUI support)${NC}"
    else
        echo "WSLg not detected - will use VcXsrv for X11"
    fi

    return $errors
}
