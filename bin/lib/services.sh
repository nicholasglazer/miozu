#!/bin/bash
# Service configuration functions for Miozu installer

# Check if running in WSL
_is_wsl() {
    grep -qi microsoft /proc/version 2>/dev/null
}

# Check if systemd is available (WSL may not have it)
_has_systemd() {
    systemctl --version &>/dev/null 2>&1
}

configure_services() {
    print_header "Configuring Services"

    # Check for systemd availability
    if ! _has_systemd; then
        echo -e "${YELLOW}Systemd not available - skipping service configuration${NC}"
        if _is_wsl; then
            echo "To enable systemd in WSL, add [boot] systemd=true to /etc/wsl.conf"
        fi
        return 0
    fi

    # Bluetooth - skip in WSL (no bluetooth hardware)
    if _is_wsl; then
        echo "Skipping bluetooth service (not available in WSL)"
    else
        if systemctl list-unit-files | grep -q bluetooth.service; then
            sudo systemctl enable --now bluetooth.service
            echo -e "${GREEN}Bluetooth service enabled${NC}"
        fi
    fi

    # Configure emacs service (works in both native and WSL with systemd)
    if ! systemctl --user is-enabled emacs.service &>/dev/null; then
        echo "Enabling emacs service (will start at next login)"
        systemctl --user enable emacs.service
    fi

    # Try starting but don't fail installation
    echo "Attempting to start emacs service..."
    systemctl --user start emacs.service || true
}