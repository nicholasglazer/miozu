#!/bin/bash
# Service configuration functions for Miozu installer

configure_services() {
    print_header "Configuring Services"
    sudo systemctl enable --now bluetooth.service

    # Configure emacs service more robustly
    if ! systemctl --user is-enabled emacs.service &>/dev/null; then
        echo "Enabling emacs service (will start at next login)"
        systemctl --user enable emacs.service
    fi

    # Try starting but don't fail installation
    echo "Attempting to start emacs service..."
    systemctl --user start emacs.service || true
}