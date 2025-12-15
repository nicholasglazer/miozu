#!/bin/bash
# Miozu WSL2 Installer
# Installs the same environment as native Linux, adapted for WSL2
#
# Key differences from native install.sh:
# - Uses WSL-specific package list (no hardware audio, bluetooth, brightness)
# - Configures X11 DISPLAY for VcXsrv/WSLg
# - Creates WSL-specific startup scripts
# - Skips hardware-dependent services
#
# Run: ./bin/install-wsl.sh

set -euo pipefail
trap 'echo -e "${RED}Error at line $LINENO${NC}"; exit 1' ERR

# Configuration
MIOZU_DIR="$HOME/.miozu"
BACKUP_DIR="$HOME/.tmp/configs_backup"
LOG_FILE="$HOME/.miozu_wsl_install.log"

# Initialize logging
exec > >(tee -a "$LOG_FILE") 2>&1

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source all library modules (shared with native installer)
source "$SCRIPT_DIR/lib/common.sh"
source "$SCRIPT_DIR/lib/packages.sh"
source "$SCRIPT_DIR/lib/config.sh"
source "$SCRIPT_DIR/lib/services.sh"
source "$SCRIPT_DIR/lib/keyboard.sh"
source "$SCRIPT_DIR/lib/wsl.sh"

# WSL-specific package installation
install_wsl_packages() {
    local pkg_type=$1
    print_header "Installing WSL $pkg_type Packages"

    local pkg_file="$MIOZU_DIR/bin/dependencies/wsl-${pkg_type}-packages.txt"

    if [[ ! -f "$pkg_file" ]]; then
        echo -e "${YELLOW}Package file not found: $pkg_file${NC}"
        echo "Falling back to standard package file..."
        pkg_file="$MIOZU_DIR/bin/dependencies/${pkg_type}-packages.txt"
    fi

    if [[ ! -f "$pkg_file" ]]; then
        echo -e "${RED}No package file found for: $pkg_type${NC}"
        return 1
    fi

    # Get available AUR helper
    local aur_helper
    aur_helper=$(get_aur_helper)

    if [[ -z "$aur_helper" ]]; then
        echo -e "${RED}No AUR helper found. Installation failed.${NC}"
        return 1
    fi

    # Filter out comments and empty lines
    local packages
    packages=$(grep -v '^#' "$pkg_file" | grep -v '^$' | tr '\n' ' ')

    if [[ -z "$packages" ]]; then
        echo "No packages to install for: $pkg_type"
        return 0
    fi

    echo "Installing packages from: $pkg_file (using $aur_helper)"
    $aur_helper -S --needed --noconfirm $packages
}

# Print banner
print_banner() {
    echo -e "${BLUE}"
    cat << 'EOF'
    ╔═══════════════════════════════════════════════════════════════╗
    ║                                                               ║
    ║     ███╗   ███╗██╗ ██████╗ ███████╗██╗   ██╗                  ║
    ║     ████╗ ████║██║██╔═══██╗╚══███╔╝██║   ██║                  ║
    ║     ██╔████╔██║██║██║   ██║  ███╔╝ ██║   ██║                  ║
    ║     ██║╚██╔╝██║██║██║   ██║ ███╔╝  ██║   ██║                  ║
    ║     ██║ ╚═╝ ██║██║╚██████╔╝███████╗╚██████╔╝                  ║
    ║     ╚═╝     ╚═╝╚═╝ ╚═════╝ ╚══════╝ ╚═════╝                   ║
    ║                                                               ║
    ║              WSL2 Edition Installer (Unified)                 ║
    ║                                                               ║
    ╚═══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
}

# Main execution
main() {
    print_banner

    echo -e "${YELLOW}Starting Miozu WSL2 Environment Installation${NC}"
    echo ""

    # WSL-specific checks
    check_wsl_prerequisites

    # Check systemd status (offer to enable)
    setup_wsl_systemd

    # Initial setup (shared with native)
    check_dependencies
    install_paru
    setup_directories
    handle_miozu_repo

    # Skip monitor hotplug (handled by _is_wsl check in config.sh)
    # setup_monitor_hotplug is skipped automatically

    # Package installation (WSL-specific lists)
    install_wsl_packages "required"

    # Optional components
    echo ""
    echo -n "Install optional packages (browsers, chat apps)? [y/N]: "
    read -n 1 -r REPLY < /dev/tty
    echo
    [[ $REPLY =~ ^[Yy]$ ]] && install_wsl_packages "optional"

    echo -n "Install developer tools (Emacs, HLS, etc.)? [y/N]: "
    read -n 1 -r REPLY < /dev/tty
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        install_wsl_packages "developer"
        setup_emacs
    fi

    # Configuration (shared with native, WSL checks built-in)
    setup_dotfiles

    # WSL-specific X11 configuration
    configure_wsl_display
    create_wsl_xinitrc
    create_wsl_start_script

    # Services (shared, skips hardware services in WSL)
    configure_services

    # Keyboard setup (shared, uses setxkbmap in WSL)
    setup_keyboard

    # Final steps
    post_install

    # Print WSL-specific instructions
    print_wsl_instructions
}

# Keep sudo alive in background
while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" || exit
done 2>/dev/null &

# Start main process
main
