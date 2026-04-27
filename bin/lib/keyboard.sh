#!/bin/bash
# Keyboard configuration functions for Miozu installer
# Supports both native Linux (localectl) and WSL (setxkbmap)

# Check if running in WSL
_is_wsl() {
    grep -qi microsoft /proc/version 2>/dev/null
}

# Check if localectl is available and functional
_has_localectl() {
    command -v localectl &>/dev/null && localectl status &>/dev/null 2>&1
}

setup_keyboard() {
    print_header "Keyboard Layout Configuration"

    echo "This system is configured for Dvorak keyboard layout by default."
    echo "If you're not a Dvorak user, you can choose QWERTY instead."
    echo
    echo "Layout options:"
    echo "1) Dvorak (default, recommended for this configuration)"
    echo "2) QWERTY (standard layout)"
    echo

    # Ensure clean input by redirecting from /dev/tty
    echo -n "Choose keyboard layout [1]: "
    read -r layout_choice < /dev/tty

    # Default to 1 if empty
    layout_choice=${layout_choice:-1}

    case $layout_choice in
        1)
            echo "Setting up Dvorak layout..."
            if setup_dvorak_layout; then
                echo -e "${GREEN}Keyboard layout setup completed successfully${NC}"
            else
                echo -e "${RED}Keyboard layout setup failed${NC}"
                echo "You may need to configure the keyboard manually after installation."
            fi
            ;;
        2)
            echo "Setting up QWERTY layout..."
            if setup_qwerty_layout; then
                echo -e "${GREEN}Keyboard layout setup completed successfully${NC}"
            else
                echo -e "${RED}Keyboard layout setup failed${NC}"
                echo "You may need to configure the keyboard manually after installation."
            fi
            ;;
        *)
            echo "Invalid choice. Defaulting to Dvorak..."
            if setup_dvorak_layout; then
                echo -e "${GREEN}Keyboard layout setup completed successfully${NC}"
            else
                echo -e "${RED}Keyboard layout setup failed${NC}"
                echo "You may need to configure the keyboard manually after installation."
            fi
            ;;
    esac

    echo
    echo "Keyboard layout configuration complete."
    echo "Changes will take effect after reboot or X11 restart."
}

# Configure keyboard via setxkbmap (for WSL or X11 sessions)
setup_keyboard_x11() {
    local layout="$1"
    local variant="$2"

    # Add setxkbmap to xinitrc
    local xinitrc="$HOME/.xinitrc"
    local setxkbmap_cmd="setxkbmap"

    if [[ -n "$variant" ]]; then
        setxkbmap_cmd="setxkbmap -layout $layout -variant $variant"
    else
        setxkbmap_cmd="setxkbmap -layout $layout"
    fi

    # Create or update xinitrc
    if [[ -f "$xinitrc" ]]; then
        # Remove old setxkbmap line if exists
        sed -i '/^setxkbmap/d' "$xinitrc"
        # Add new line after shebang
        sed -i "2a\\$setxkbmap_cmd" "$xinitrc"
    else
        # Create new xinitrc
        cat > "$xinitrc" << EOF
#!/bin/bash
$setxkbmap_cmd
exec xmonad
EOF
        chmod +x "$xinitrc"
    fi

    echo -e "${GREEN}X11 keyboard configured via setxkbmap${NC}"

    # Also add to fish config for X sessions
    local fish_config="$HOME/.config/fish/config.fish"
    if [[ -f "$fish_config" ]]; then
        if ! grep -q "setxkbmap.*$layout" "$fish_config"; then
            cat >> "$fish_config" << EOF

# Keyboard layout for X11 (set on session start)
if test -n "\$DISPLAY"
    $setxkbmap_cmd 2>/dev/null
end
EOF
            echo -e "${GREEN}Added keyboard layout to Fish config${NC}"
        fi
    fi
}

# Install Wayland XKB environment for systemd user session.
# Symlinks the repo-canonical environment.d file into ~/.config/environment.d/
# so every Wayland compositor launched through the user session (teruwm,
# Hyprland, Sway, GNOME) picks up us,ua + dvorak + alt_shift_toggle.
#
# For non-Dvorak users, install_wayland_override writes a numerically-higher
# file (20-user-keyboard.conf) which systemd loads *after* 10-* and thus wins.
# Layered overrides are systemd's native pattern for environment.d.
setup_keyboard_wayland() {
    local src="$MIOZU_DIR/.config/environment.d/10-miozu-keyboard.conf"
    local dst="$HOME/.config/environment.d/10-miozu-keyboard.conf"

    if [[ ! -f "$src" ]]; then
        echo -e "${YELLOW}Warning: Wayland XKB env file not found: $src${NC}"
        return 0
    fi

    mkdir -p "$HOME/.config/environment.d"
    if ln -sf "$src" "$dst"; then
        echo -e "${GREEN}Wayland keyboard env (miozu default) installed: $dst${NC}"
    else
        echo -e "${RED}Failed to symlink Wayland keyboard env${NC}"
    fi
}

# Layered user override. Writes ~/.config/environment.d/20-user-keyboard.conf
# when layout/variant/options differ from the miozu default (us,ua / dvorak, /
# grp:alt_shift_toggle,caps:escape). Removes any stale override otherwise.
#
# systemd environment.d loads files in alphabetical order; higher numeric
# prefix wins. So 20-user-keyboard.conf overrides 10-miozu-keyboard.conf.
# This is identical to how /etc/environment.d/ and xdg config directories
# are normally layered — no custom mechanism.
#
# Args: layout variant options  (empty string = unset that field)
install_wayland_override() {
    local layout="$1"
    local variant="$2"
    local options="$3"
    local override="$HOME/.config/environment.d/20-user-keyboard.conf"
    local default_layout="us,ua"
    local default_variant="dvorak,"
    local default_options="grp:alt_shift_toggle,caps:escape"

    if [[ "$layout" == "$default_layout" && "$variant" == "$default_variant" && "$options" == "$default_options" ]]; then
        if [[ -f "$override" ]]; then
            rm -f "$override" && echo -e "${GREEN}Removed stale Wayland keyboard override (miozu default is active)${NC}"
        fi
        return 0
    fi

    mkdir -p "$HOME/.config/environment.d"
    cat >"$override" <<EOF
# User keyboard override — takes precedence over 10-miozu-keyboard.conf
# because systemd environment.d loads files in alphabetical order and a
# higher numeric prefix wins. Edit or delete this file to change layout.
# Generated by bin/lib/keyboard.sh on $(date -u +%Y-%m-%dT%H:%M:%SZ).

XKB_DEFAULT_LAYOUT=${layout}
XKB_DEFAULT_VARIANT=${variant}
XKB_DEFAULT_OPTIONS=${options}
EOF
    echo -e "${GREEN}Wayland keyboard override installed: $override${NC}"
    echo -e "${YELLOW}Log out and back in (or restart your compositor) to activate.${NC}"
}

setup_dvorak_layout() {
    echo "Configuring Dvorak keyboard layout..."

    if _is_wsl; then
        # WSL: Use setxkbmap instead of localectl
        echo "WSL detected - using setxkbmap for keyboard configuration"
        setup_keyboard_x11 "us" "dvorak"
        return 0
    fi

    # Native Linux: Use localectl if available
    if _has_localectl; then
        # Set console (vconsole) keymap
        echo "Setting console keymap to Dvorak..."
        if sudo localectl set-keymap dvorak; then
            echo -e "${GREEN}Console keymap set to Dvorak${NC}"
        else
            echo -e "${RED}Failed to set console keymap${NC}"
            return 1
        fi

        # Set X11 keymap
        echo "Setting X11 keymap to Dvorak..."
        if sudo localectl set-x11-keymap us pc105 dvorak; then
            echo -e "${GREEN}X11 keymap set to Dvorak${NC}"
        else
            echo -e "${RED}Failed to set X11 keymap${NC}"
            return 1
        fi
    else
        echo "localectl not available - using setxkbmap"
        setup_keyboard_x11 "us" "dvorak"
    fi

    # Ensure X11 directory exists
    sudo mkdir -p /etc/X11/xorg.conf.d

    # Install touchpad configuration (native only)
    if ! _is_wsl; then
        if [ -f "$MIOZU_DIR/X11/xorg.conf.d/30-touchpad.conf" ]; then
            echo "Installing touchpad configuration..."
            if sudo ln -sf "$MIOZU_DIR/X11/xorg.conf.d/30-touchpad.conf" /etc/X11/xorg.conf.d/30-touchpad.conf; then
                echo -e "${GREEN}Touchpad configuration installed (tap-to-click enabled)${NC}"
            else
                echo -e "${RED}Failed to install touchpad configuration${NC}"
            fi
        fi
    fi

    # Symlink the Dvorak X11 config
    if [ -f "$MIOZU_DIR/X11/xorg.conf.d/00-keyboard.conf" ]; then
        echo "Installing Dvorak X11 configuration..."
        if sudo ln -sf "$MIOZU_DIR/X11/xorg.conf.d/00-keyboard.conf" /etc/X11/xorg.conf.d/00-keyboard.conf; then
            echo -e "${GREEN}Dvorak X11 configuration installed${NC}"
        else
            echo -e "${RED}Failed to install X11 configuration${NC}"
            return 1
        fi
    else
        echo -e "${YELLOW}Warning: Dvorak X11 config not found in repository${NC}"
        echo "Path checked: $MIOZU_DIR/X11/xorg.conf.d/00-keyboard.conf"
    fi

    # Install Wayland XKB env (teruwm + any other Wayland compositor)
    setup_keyboard_wayland
    # Dvorak user: miozu default matches; remove any stale override.
    install_wayland_override "us,ua" "dvorak," "grp:alt_shift_toggle,caps:escape"

    # Verify final configuration (native only)
    if _has_localectl && localectl status | grep -q "Keymap.*dvorak"; then
        echo -e "${GREEN}Dvorak layout configured successfully${NC}"
    elif _is_wsl; then
        echo -e "${GREEN}Dvorak layout configured for X11 sessions${NC}"
    else
        echo -e "${YELLOW}Warning: Dvorak keymap may not be fully configured${NC}"
    fi
}

setup_qwerty_layout() {
    echo "Configuring QWERTY keyboard layout..."

    if _is_wsl; then
        # WSL: Use setxkbmap instead of localectl
        echo "WSL detected - using setxkbmap for keyboard configuration"
        setup_keyboard_x11 "us" ""
        echo -e "${YELLOW}Note: Some keybindings in XMonad are optimized for Dvorak layout.${NC}"
        return 0
    fi

    # Native Linux: Use localectl if available
    if _has_localectl; then
        # Set console (vconsole) keymap
        echo "Setting console keymap to QWERTY..."
        if sudo localectl set-keymap us; then
            echo -e "${GREEN}Console keymap set to QWERTY${NC}"
        else
            echo -e "${RED}Failed to set console keymap${NC}"
            return 1
        fi

        # Set X11 keymap
        echo "Setting X11 keymap to QWERTY..."
        if sudo localectl set-x11-keymap us pc105; then
            echo -e "${GREEN}X11 keymap set to QWERTY${NC}"
        else
            echo -e "${RED}Failed to set X11 keymap${NC}"
            return 1
        fi
    else
        echo "localectl not available - using setxkbmap"
        setup_keyboard_x11 "us" ""
    fi

    # Install touchpad configuration (native only)
    if ! _is_wsl; then
        if [ -f "$MIOZU_DIR/X11/xorg.conf.d/30-touchpad.conf" ]; then
            echo "Installing touchpad configuration..."
            if sudo ln -sf "$MIOZU_DIR/X11/xorg.conf.d/30-touchpad.conf" /etc/X11/xorg.conf.d/30-touchpad.conf; then
                echo -e "${GREEN}Touchpad configuration installed (tap-to-click enabled)${NC}"
            else
                echo -e "${RED}Failed to install touchpad configuration${NC}"
            fi
        fi
    fi

    # Remove any existing X11 keyboard config (will use system default)
    if [ -L "/etc/X11/xorg.conf.d/00-keyboard.conf" ]; then
        echo "Removing custom X11 keyboard configuration..."
        if sudo rm -f /etc/X11/xorg.conf.d/00-keyboard.conf; then
            echo -e "${GREEN}Custom X11 keyboard configuration removed${NC}"
        else
            echo -e "${RED}Failed to remove X11 configuration${NC}"
            return 1
        fi
    fi

    # Install Wayland XKB env (miozu default) + QWERTY override so teruwm
    # et al. don't inherit the repo's Dvorak+UA defaults.
    setup_keyboard_wayland
    install_wayland_override "us" "" ""

    # Verify final configuration
    if _has_localectl && localectl status | grep -q "Keymap.*us"; then
        echo -e "${GREEN}QWERTY layout configured successfully${NC}"
        echo -e "${YELLOW}Note: Some keybindings in XMonad are optimized for Dvorak layout.${NC}"
    elif _is_wsl; then
        echo -e "${GREEN}QWERTY layout configured for X11 sessions${NC}"
        echo -e "${YELLOW}Note: Some keybindings in XMonad are optimized for Dvorak layout.${NC}"
    else
        echo -e "${YELLOW}Warning: QWERTY keymap may not be fully configured${NC}"
    fi
}
