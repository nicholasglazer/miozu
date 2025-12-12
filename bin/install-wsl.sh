#!/bin/bash
# Miozu WSL2 Installer
# For Arch Linux on WSL2 with VcXsrv
# Run: ./bin/install-wsl.sh

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

MIOZU_DIR="$HOME/.miozu"

print_header() {
    echo -e "\n${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${YELLOW}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}\n"
}

print_step() {
    echo -e "${GREEN}→${NC} $1"
}

print_warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

# Check if running in WSL
check_wsl() {
    if ! grep -qi microsoft /proc/version 2>/dev/null; then
        print_error "This script is designed for WSL2. Detected non-WSL environment."
        echo "For native Linux, use: ./bin/install.sh"
        exit 1
    fi
    print_step "WSL2 environment detected"
}

# Install packages
install_packages() {
    print_header "Installing Packages"

    # Check paru
    if ! command -v paru &> /dev/null; then
        print_step "Installing paru..."
        sudo pacman -S --needed --noconfirm base-devel git
        git clone https://aur.archlinux.org/paru.git /tmp/paru
        cd /tmp/paru && makepkg -si --noconfirm
        cd - > /dev/null
    fi

    print_step "Installing X11 + XMonad packages..."
    paru -S --needed --noconfirm \
        xorg-server \
        xorg-xinit \
        xorg-xrandr \
        xorg-xsetroot \
        xorg-xset \
        xdotool \
        xclip \
        xmonad \
        xmonad-contrib \
        xmobar \
        haskell-dbus

    print_step "Installing terminal and essentials..."
    paru -S --needed --noconfirm \
        alacritty \
        fish \
        neovim \
        rofi \
        dunst \
        feh

    print_step "Installing fonts..."
    paru -S --needed --noconfirm \
        ttf-roboto \
        ttf-ibm-plex \
        noto-fonts-emoji \
        ttf-nerd-fonts-symbols \
        ttf-font-awesome \
        otf-ipafont

    print_step "Installing dev tools..."
    paru -S --needed --noconfirm \
        ripgrep \
        fd \
        nnn \
        ncdu \
        git \
        openssh \
        aria2 \
        zip \
        unzip \
        wget \
        curl
}

# Install Haskell Stack
install_stack() {
    print_header "Installing Haskell Stack"

    if command -v stack &> /dev/null; then
        print_step "Stack already installed"
    else
        print_step "Installing stack..."
        curl -sSL https://get.haskellstack.org/ | sh
    fi
}

# Setup dotfiles
setup_dotfiles() {
    print_header "Setting Up Dotfiles"

    print_step "Creating config directories..."
    mkdir -p ~/.config/{xmonad/lib,xmonad/xmobar,fish,doom/themes,rofi/colors,dunst,alacritty,fontconfig,nvim}

    print_step "Symlinking XMonad config..."
    ln -sf "$MIOZU_DIR/.config/xmonad/xmonad.hs" ~/.config/xmonad/
    for f in "$MIOZU_DIR"/.config/xmonad/lib/*.hs; do
        [ -f "$f" ] && ln -sf "$f" ~/.config/xmonad/lib/
    done
    for f in "$MIOZU_DIR"/.config/xmonad/xmobar/*.hs; do
        [ -f "$f" ] && ln -sf "$f" ~/.config/xmonad/xmobar/
    done

    print_step "Symlinking Fish config..."
    ln -sf "$MIOZU_DIR/.config/fish/config.fish" ~/.config/fish/

    print_step "Symlinking Rofi config..."
    [ -f "$MIOZU_DIR/.config/rofi/miozu.rasi" ] && ln -sf "$MIOZU_DIR/.config/rofi/miozu.rasi" ~/.config/rofi/config.rasi
    for f in "$MIOZU_DIR"/.config/rofi/colors/*.rasi; do
        [ -f "$f" ] && ln -sf "$f" ~/.config/rofi/colors/
    done

    print_step "Symlinking Dunst config..."
    [ -f "$MIOZU_DIR/.config/dunst/dunstrc" ] && ln -sf "$MIOZU_DIR/.config/dunst/dunstrc" ~/.config/dunst/

    print_step "Symlinking Fontconfig..."
    [ -f "$MIOZU_DIR/.config/fontconfig/fonts.conf" ] && ln -sf "$MIOZU_DIR/.config/fontconfig/fonts.conf" ~/.config/fontconfig/

    print_step "Symlinking Neovim config..."
    [ -f "$MIOZU_DIR/.config/nvim/.vimrc" ] && ln -sf "$MIOZU_DIR/.config/nvim/.vimrc" ~/.config/nvim/init.vim
}

# Configure shell for WSL2 X11
configure_shell() {
    print_header "Configuring Shell for X11"

    # Fish config
    if [ -f ~/.config/fish/config.fish ]; then
        if ! grep -q "DISPLAY.*nameserver" ~/.config/fish/config.fish; then
            print_step "Adding X11 DISPLAY to Fish config..."
            cat >> ~/.config/fish/config.fish << 'EOF'

# WSL2 X11 Display for VcXsrv
if test -f /etc/resolv.conf
    set -gx DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
end
set -gx LIBGL_ALWAYS_SOFTWARE 1
EOF
        else
            print_step "Fish X11 config already present"
        fi
    fi

    # Bash config (backup)
    if ! grep -q "DISPLAY.*nameserver" ~/.bashrc 2>/dev/null; then
        print_step "Adding X11 DISPLAY to Bash config..."
        cat >> ~/.bashrc << 'EOF'

# WSL2 X11 Display for VcXsrv
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_SOFTWARE=1
EOF
    fi
}

# Create xinitrc
create_xinitrc() {
    print_header "Creating X Startup Script"

    print_step "Writing ~/.xinitrc..."
    cat > ~/.xinitrc << 'EOF'
#!/bin/bash

# Start dbus session
eval $(dbus-launch --sh-syntax)
export DBUS_SESSION_BUS_ADDRESS

# Set cursor
xsetroot -cursor_name left_ptr

# Set background
xsetroot -solid "#1a1a2e"

# Start notification daemon
dunst &

# Start XMonad
exec xmonad
EOF
    chmod +x ~/.xinitrc
}

# Setup Doom Emacs
setup_doom() {
    print_header "Setting Up Doom Emacs"

    if [ -d ~/.emacs.d ]; then
        print_warn "~/.emacs.d already exists, skipping Doom clone"
    else
        print_step "Cloning Doom Emacs..."
        git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
    fi

    print_step "Symlinking Doom config..."
    mkdir -p ~/.config/doom
    [ -f "$MIOZU_DIR/.config/doom/init.el" ] && ln -sf "$MIOZU_DIR/.config/doom/init.el" ~/.config/doom/
    [ -f "$MIOZU_DIR/.config/doom/config.el" ] && ln -sf "$MIOZU_DIR/.config/doom/config.el" ~/.config/doom/
    [ -f "$MIOZU_DIR/.config/doom/packages.el" ] && ln -sf "$MIOZU_DIR/.config/doom/packages.el" ~/.config/doom/

    print_step "Cloning Miozu theme..."
    mkdir -p ~/.config/doom/themes
    if [ ! -d ~/.config/doom/themes/miozu ]; then
        git clone https://github.com/miozutheme/doom-miozu.git ~/.config/doom/themes/miozu || true
    fi

    echo ""
    print_warn "Run manually after install: ~/.emacs.d/bin/doom install"
    print_warn "Then: ~/.emacs.d/bin/doom sync"
}

# Build XMonad
build_xmonad() {
    print_header "Building XMonad"

    if [ -f "$MIOZU_DIR/bin/xmonad.sh" ]; then
        print_step "Building with Miozu xmonad.sh..."
        cd "$MIOZU_DIR"
        ./bin/xmonad.sh || {
            print_warn "Stack build failed, trying system xmonad..."
            xmonad --recompile || true
        }
    else
        print_step "Compiling XMonad..."
        xmonad --recompile || true
    fi

    # Add local bin to path
    mkdir -p ~/.local/bin
    if ! grep -q '.local/bin' ~/.bashrc; then
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.bashrc
    fi
}

# Set default shell
set_shell() {
    print_header "Setting Default Shell"

    if [ "$SHELL" != "$(which fish)" ]; then
        print_step "Setting Fish as default shell..."
        chsh -s "$(which fish)" || print_warn "Could not change shell, do it manually: chsh -s $(which fish)"
    else
        print_step "Fish is already default shell"
    fi
}

# Rebuild font cache
rebuild_fonts() {
    print_header "Rebuilding Font Cache"
    fc-cache -fv
}

# Print final instructions
print_instructions() {
    print_header "Setup Complete!"

    echo -e "${GREEN}WSL2 side is ready!${NC}\n"

    echo "Next steps on Windows:"
    echo "━━━━━━━━━━━━━━━━━━━━━━"
    echo "1. Install VcXsrv:"
    echo "   winget install marha.VcXsrv"
    echo ""
    echo "2. Create xmonad-fullscreen.xlaunch (see docs/WSL2_XMONAD_SETUP.md)"
    echo ""
    echo "3. Add firewall rule for VcXsrv"
    echo ""

    echo "To start XMonad:"
    echo "━━━━━━━━━━━━━━━━"
    echo "1. On Windows: Double-click xmonad-fullscreen.xlaunch"
    echo "2. In WSL: startx"
    echo ""

    echo "Don't forget:"
    echo "━━━━━━━━━━━━━"
    echo "• ~/.emacs.d/bin/doom install  (if you want Emacs)"
    echo "• Review docs/WSL2_XMONAD_SETUP.md for troubleshooting"
    echo ""

    echo -e "${YELLOW}Full guide: ~/.miozu/docs/WSL2_XMONAD_SETUP.md${NC}"
}

# Main
main() {
    echo -e "${BLUE}"
    echo "╔═══════════════════════════════════════════════════════════════╗"
    echo "║                                                               ║"
    echo "║     ███╗   ███╗██╗ ██████╗ ███████╗██╗   ██╗                  ║"
    echo "║     ████╗ ████║██║██╔═══██╗╚══███╔╝██║   ██║                  ║"
    echo "║     ██╔████╔██║██║██║   ██║  ███╔╝ ██║   ██║                  ║"
    echo "║     ██║╚██╔╝██║██║██║   ██║ ███╔╝  ██║   ██║                  ║"
    echo "║     ██║ ╚═╝ ██║██║╚██████╔╝███████╗╚██████╔╝                  ║"
    echo "║     ╚═╝     ╚═╝╚═╝ ╚═════╝ ╚══════╝ ╚═════╝                   ║"
    echo "║                                                               ║"
    echo "║                    WSL2 Edition Installer                     ║"
    echo "║                                                               ║"
    echo "╚═══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"

    check_wsl

    install_packages
    install_stack
    setup_dotfiles
    configure_shell
    create_xinitrc
    setup_doom
    build_xmonad
    set_shell
    rebuild_fonts
    print_instructions
}

main "$@"
