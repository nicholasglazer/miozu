# -- Nicholas Glazer <glazer.nicholas@gmail.com> --


# --- Functions
# Keybindings
function fish_user_key_bindings
    bind \eu forward-word # autocomplete suggented by 1 word per use
    bind \cu forward-char # autocomplete suggented line
end

# --- Executables
# This command ensures that /usr/local/bin is searched first when looking for executable programs
set -x PATH /usr/local/bin $PATH

# Ghostty shell integration
if test "$GHOSTTY_RESOURCES_DIR"
    source "$GHOSTTY_RESOURCES_DIR/shell-integration/fish/vendor_conf.d/ghostty-shell-integration.fish"
end
# Make doom-emacs as a var
set -x PATH $HOME/.emacs.d/bin $PATH
# Set an editor
set -x EDITOR "nvim"
# Comments below will be substituted with miozu variables automatically after you run ./install.sh
set -x MIOZU_DIR /home/n/.miozu
# INSERT MIOZU GLOBAL

# --- SSH Agent (Fish-compatible)
# Start ssh-agent if not running
if test -z "$SSH_AUTH_SOCK"
    eval (ssh-agent -c)
    ssh-add ~/.ssh/id_ng 2>/dev/null
end

# --- Internal settings
# Set fish-specific variables
# Remove fish greeting -U will need to run it once to remove completely
set -x fish_greeting ""
# set -U fish_greeting

# pnpm
set -gx PNPM_HOME "/home/n/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end

# --- Aliases
# Please allow me ;)
alias pls 'sudo'
# List files with details and color
alias l 'ls -alhF'
# Shortcut for systemctl
alias s 'systemctl'
# Enable colored output for grep
alias grep 'grep --color=auto'
# Removing dir but preserving /root from deleting
alias rmf 'rm –preserve-root -Irfv'
# Navigate up one directory
alias .. 'cd ..'
# Violent way to close an app
alias k 'killall -9'
# Clear the terminal screen
alias c 'clear'
# Create a new file
alias t 'touch'
# Create a new directory and its parent directories if necessary
alias md 'mkdir -p'
# Ping the Arch Linux website with 3 packets
alias ping 'ping -c 3 https://archlinux.org'
# Open files in Neovim
alias vi 'nvim'
# Show all running services for the whole system
alias srs 'sudo systemctl list-units --type=service --state=running'
# Show all failed services for the whole system
alias sfs 'sudo systemctl --failed'
# Show all running services for the user
alias urs 'systemctl --user list-units --type=service --state=running'
# Show all failed services for the user
alias ufs 'systemctl --user --failed'
# Recompile xmonad
alias xr 'xmonad --recompile'
# Git clone
alias gc 'git clone'
# Run htop
alias h 'htop'
# Make a symlink
alias lns 'ln -si'
# Remove package
alias remove 'sudo pacman -Rsc'
# Qick move to miozu dir
alias m "cd $MIOZU_DIR"

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH

# android path
set -gx ANDROID_HOME "$HOME/code/Android/Sdk"
set -gx PATH $PATH "$ANDROID_HOME/tools" "$ANDROID_HOME/platform-tools"
set -gx JAVA_HOME /opt/android-studio/jbr
set -gx ANDROID_HOME $HOME/Android/Sdk
set -gx NDK_HOME $ANDROID_HOME/ndk/(ls -1 $ANDROID_HOME/ndk | tail -1)
