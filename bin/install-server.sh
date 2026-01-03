#!/bin/bash
# Miozu Server Installer
# Minimal production server setup for Arch Linux
# Last updated: 2026-01-03
# Author: Nicholas Glazer <glazer.nicholas@gmail.com>

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MIOZU_DIR="$(dirname "$SCRIPT_DIR")"
LOG_FILE="$HOME/.miozu_server_install.log"
BACKUP_DIR="$HOME/.miozu_backup_$(date +%Y%m%d_%H%M%S)"

# Global state
CONFIGURED_SSH_PORT=""
SERVER_IP=""

log() { echo -e "${GREEN}[+]${NC} $1"; }
warn() { echo -e "${YELLOW}[!]${NC} $1"; }
err() { echo -e "${RED}[x]${NC} $1"; }

# Cleanup on failure
cleanup_on_error() {
    local exit_code=$?
    if [[ $exit_code -ne 0 ]]; then
        err "Script failed at line $1 (exit code: $exit_code)"
        err "Check log: $LOG_FILE"
        err "Backups at: $BACKUP_DIR"
    fi
}
trap 'cleanup_on_error $LINENO' ERR

# --- Helper: Get current SSH port ---
get_current_ssh_port() {
    local port
    # Try ss with sudo for process visibility
    port=$(sudo ss -tlnp 2>/dev/null | grep sshd | grep -oP ':\K[0-9]+' | head -1)
    if [[ -z "$port" ]]; then
        # Fallback: read from config
        port=$(grep -h "^Port" /etc/ssh/sshd_config /etc/ssh/sshd_config.d/*.conf 2>/dev/null | tail -1 | awk '{print $2}')
    fi
    echo "${port:-22}"
}

# --- Helper: Get server IP ---
get_server_ip() {
    ip -4 addr show scope global | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | head -1
}

# --- Pre-flight Safety Checks ---
preflight_checks() {
    echo -e "${YELLOW}=== Pre-flight Safety Checks ===${NC}"
    
    local errors=0
    
    # Check if running as root (bad)
    if [[ $EUID -eq 0 ]]; then
        err "Don't run as root - run as your normal user with sudo access"
        exit 1
    fi
    
    # Check sudo access
    if ! sudo -v 2>/dev/null; then
        err "No sudo access. Add user to wheel group first."
        exit 1
    fi
    log "Sudo access: OK"
    
    # Check SSH key exists (fixed glob expansion)
    local has_key=false
    [[ -f ~/.ssh/authorized_keys ]] && has_key=true
    for f in ~/.ssh/id_*.pub; do
        [[ -f "$f" ]] && has_key=true && break
    done
    
    if [[ "$has_key" == "false" ]]; then
        err "No SSH key found in ~/.ssh/"
        err "Set up SSH keys BEFORE running this script!"
        err "Run: ssh-keygen -t ed25519"
        ((errors++))
    else
        log "SSH key found"
    fi
    
    # Get and display server info
    SERVER_IP=$(get_server_ip)
    local current_port=$(get_current_ssh_port)
    log "Server IP: $SERVER_IP"
    log "Current SSH port: $current_port"
    
    # Warn about keeping session open
    echo ""
    warn "IMPORTANT: Keep this SSH session open until you verify new connection works!"
    echo ""
    
    if [[ $errors -gt 0 ]]; then
        err "Pre-flight checks failed. Fix issues above first."
        exit 1
    fi
    
    read -p "Continue with installation? [y/N]: " reply
    [[ ! $reply =~ ^[Yy]$ ]] && exit 0
}

# --- Backup existing configs ---
backup_configs() {
    log "Backing up existing configs to $BACKUP_DIR"
    mkdir -p "$BACKUP_DIR"
    
    # Backup SSH configs
    [[ -f /etc/ssh/sshd_config ]] && sudo cp /etc/ssh/sshd_config "$BACKUP_DIR/"
    if [[ -d /etc/ssh/sshd_config.d ]]; then
        sudo cp -r /etc/ssh/sshd_config.d "$BACKUP_DIR/"
    fi
    
    # Backup firewall
    [[ -f /etc/nftables.conf ]] && sudo cp /etc/nftables.conf "$BACKUP_DIR/"
    
    # Backup sysctl
    for f in /etc/sysctl.d/*.conf; do
        [[ -f "$f" ]] && sudo cp "$f" "$BACKUP_DIR/" 2>/dev/null || true
    done
    
    # Make backup readable
    sudo chown -R "$(whoami)" "$BACKUP_DIR"
    
    log "Backup complete"
}

# --- AUR Helper ---
install_paru() {
    if command -v paru &>/dev/null; then
        log "paru already installed"
        return 0
    fi
    
    log "Installing paru..."
    sudo pacman -S --noconfirm --needed base-devel git
    
    local temp_dir
    temp_dir=$(mktemp -d)
    git clone https://aur.archlinux.org/paru-bin.git "$temp_dir"
    (cd "$temp_dir" && makepkg -si --noconfirm)
    rm -rf "$temp_dir"
    log "paru installed"
}

# --- Package Installation ---
install_packages() {
    log "Installing server packages..."
    local pkg_file="$SCRIPT_DIR/dependencies/server-packages.txt"
    
    if [[ ! -f "$pkg_file" ]]; then
        err "Package file not found: $pkg_file"
        exit 1
    fi
    
    # Read packages into array (handles spaces/special chars)
    local -a packages=()
    while IFS= read -r line; do
        [[ "$line" =~ ^#.*$ ]] && continue  # Skip comments
        [[ -z "$line" ]] && continue         # Skip empty lines
        packages+=("$line")
    done < "$pkg_file"
    
    if [[ ${#packages[@]} -eq 0 ]]; then
        warn "No packages to install"
        return 0
    fi
    
    paru -S --needed --noconfirm "${packages[@]}"
    log "Packages installed"
}

# --- SSH Hardening (SAFE) ---
setup_ssh() {
    log "Configuring SSH hardening..."
    
    local ssh_config="/etc/ssh/sshd_config.d/10-hardening.conf"
    
    # Verify sshd_config.d is supported
    if ! grep -q "Include.*/etc/ssh/sshd_config.d" /etc/ssh/sshd_config 2>/dev/null; then
        warn "sshd_config.d not enabled in sshd_config, adding Include directive"
        echo "Include /etc/ssh/sshd_config.d/*.conf" | sudo tee -a /etc/ssh/sshd_config > /dev/null
    fi
    sudo mkdir -p /etc/ssh/sshd_config.d
    
    # Check for existing authorized_keys
    if [[ ! -f ~/.ssh/authorized_keys ]]; then
        err "No ~/.ssh/authorized_keys found!"
        err "Add your public key first, then re-run"
        return 1
    fi
    
    if [[ -f "$ssh_config" ]]; then
        warn "SSH hardening config exists"
        read -p "Overwrite? [y/N]: " reply
        [[ ! $reply =~ ^[Yy]$ ]] && return 0
    fi
    
    # Get current SSH port
    local current_port
    current_port=$(get_current_ssh_port)
    
    # Prompt for SSH port with validation
    while true; do
        read -p "SSH port [$current_port]: " SSH_PORT
        SSH_PORT=${SSH_PORT:-$current_port}
        
        # Validate port number
        if [[ "$SSH_PORT" =~ ^[0-9]+$ ]] && [[ "$SSH_PORT" -ge 1 ]] && [[ "$SSH_PORT" -le 65535 ]]; then
            break
        else
            err "Invalid port. Enter a number between 1-65535"
        fi
    done
    
    # Prompt for allowed users
    read -p "Allowed SSH users [$(whoami) root]: " SSH_USERS
    SSH_USERS=${SSH_USERS:-"$(whoami) root"}
    
    # Create config but DON'T restart yet
    sudo tee "$ssh_config" > /dev/null << SSHEOF
Port $SSH_PORT
PermitRootLogin without-password
PasswordAuthentication no
PubkeyAuthentication yes
MaxAuthTries 3
MaxSessions 3
LoginGraceTime 30
ClientAliveInterval 300
ClientAliveCountMax 2
X11Forwarding no
AllowAgentForwarding no
AllowTcpForwarding no
AllowUsers $SSH_USERS
SSHEOF
    
    log "SSH config written (port $SSH_PORT, users: $SSH_USERS)"
    warn "SSH will be restarted AFTER firewall is configured"
    
    # Set global for firewall function
    CONFIGURED_SSH_PORT="$SSH_PORT"
}

# --- Firewall ---
setup_firewall() {
    log "Configuring nftables firewall..."
    
    # Verify nftables is installed
    if ! command -v nft &>/dev/null; then
        err "nftables not installed. Install packages first."
        return 1
    fi
    
    # Determine SSH port
    local ssh_port="${CONFIGURED_SSH_PORT:-}"
    if [[ -z "$ssh_port" ]]; then
        ssh_port=$(get_current_ssh_port)
    fi
    
    log "Configuring firewall for SSH port $ssh_port"
    
    sudo tee /etc/nftables.conf > /dev/null << NFTEOF
#!/usr/bin/nft -f
flush ruleset

table inet filter {
  chain input {
    type filter hook input priority filter; policy drop;
    ct state invalid drop
    ct state {established, related} accept
    iif lo accept
    ip protocol icmp limit rate 10/second burst 5 packets accept
    meta l4proto ipv6-icmp limit rate 10/second burst 5 packets accept
    tcp dport $ssh_port ct state new meter ssh-flood { ip saddr limit rate over 15/minute burst 10 packets } drop
    tcp dport $ssh_port ct state new accept
    iifname "docker0" accept
    iifname "br-*" accept
    reject with icmpx type admin-prohibited
  }
  chain forward {
    type filter hook forward priority filter; policy accept;
  }
  chain output {
    type filter hook output priority filter; policy accept;
  }
}
NFTEOF
    
    sudo systemctl enable --now nftables
    sudo nft -f /etc/nftables.conf
    log "Firewall configured for port $ssh_port"
    
    # Restart CrowdSec bouncer if running (flush ruleset wipes its tables)
    if systemctl is-active --quiet crowdsec-firewall-bouncer 2>/dev/null; then
        sudo systemctl restart crowdsec-firewall-bouncer
    fi
    
    # NOW restart SSH (firewall is ready)
    warn "Restarting SSH daemon..."
    sudo systemctl restart sshd
    
    echo ""
    echo -e "${YELLOW}╔══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║  TEST NOW - Open a NEW terminal and run:                 ║${NC}"
    echo -e "${YELLOW}║                                                          ║${NC}"
    echo -e "${YELLOW}║  ssh -p $ssh_port $(whoami)@$SERVER_IP${NC}"
    echo -e "${YELLOW}║                                                          ║${NC}"
    echo -e "${YELLOW}║  DO NOT close this session until new connection works!   ║${NC}"
    echo -e "${YELLOW}╚══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    
    read -p "Did the new connection work? [y/N]: " reply
    if [[ ! $reply =~ ^[Yy]$ ]]; then
        err "Rolling back SSH and firewall..."
        
        # Restore SSH config
        if [[ -f "$BACKUP_DIR/sshd_config.d/10-hardening.conf" ]]; then
            sudo cp "$BACKUP_DIR/sshd_config.d/10-hardening.conf" /etc/ssh/sshd_config.d/
        else
            sudo rm -f /etc/ssh/sshd_config.d/10-hardening.conf
        fi
        
        # Restore firewall
        if [[ -f "$BACKUP_DIR/nftables.conf" ]]; then
            sudo cp "$BACKUP_DIR/nftables.conf" /etc/nftables.conf
            sudo nft -f /etc/nftables.conf
        else
            # No backup = flush to allow all
            sudo nft flush ruleset
        fi
        
        sudo systemctl restart sshd
        err "Rolled back. Check your SSH key setup."
        exit 1
    fi
    
    log "SSH + Firewall verified working!"
}

# --- CrowdSec ---
setup_crowdsec() {
    log "Configuring CrowdSec..."
    
    if ! command -v cscli &>/dev/null; then
        warn "CrowdSec not installed, skipping"
        return 0
    fi
    
    sudo systemctl enable --now crowdsec
    sudo systemctl enable --now crowdsec-firewall-bouncer
    
    # Install collections (ignore errors for already installed)
    local collections=(
        crowdsecurity/linux
        crowdsecurity/sshd
        crowdsecurity/nginx
        crowdsecurity/http-cve
        crowdsecurity/base-http-scenarios
    )
    
    for col in "${collections[@]}"; do
        sudo cscli collections install "$col" --yes 2>/dev/null || true
    done
    
    # Enable Docker chain
    local bouncer_config="/etc/crowdsec/bouncers/crowdsec-firewall-bouncer.yaml"
    if [[ -f "$bouncer_config" ]]; then
        sudo sed -i 's/#  - DOCKER-USER/  - DOCKER-USER/' "$bouncer_config" 2>/dev/null || true
    fi
    
    sudo systemctl reload crowdsec
    sudo systemctl restart crowdsec-firewall-bouncer
    log "CrowdSec configured"
}

# --- Kernel Hardening ---
setup_kernel_hardening() {
    log "Applying kernel hardening..."
    
    sudo tee /etc/sysctl.d/99-security-hardening.conf > /dev/null << 'SYSEOF'
# Network security
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.all.send_redirects = 0
net.ipv4.conf.default.send_redirects = 0
net.ipv4.conf.all.log_martians = 1
net.ipv4.conf.default.log_martians = 1

# TCP hardening
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_rfc1337 = 1
net.ipv4.tcp_fin_timeout = 30
net.ipv4.tcp_max_syn_backlog = 8192
net.core.somaxconn = 8192

# Kernel hardening
kernel.kptr_restrict = 2
kernel.dmesg_restrict = 1
kernel.perf_event_paranoid = 3
kernel.kexec_load_disabled = 1
SYSEOF
    
    sudo sysctl --system > /dev/null
    log "Kernel hardened"
}

# --- Docker ---
setup_docker() {
    log "Configuring Docker..."
    
    if ! command -v docker &>/dev/null; then
        warn "Docker not installed, skipping"
        return 0
    fi
    
    sudo systemctl enable --now docker
    sudo usermod -aG docker "$(whoami)" 2>/dev/null || true
    log "Docker configured (re-login for group to take effect)"
}

# --- Services ---
setup_services() {
    log "Enabling services..."
    
    # Cronie
    if command -v crond &>/dev/null || [[ -f /usr/lib/systemd/system/cronie.service ]]; then
        sudo systemctl enable --now cronie
    fi
    
    # Haveged
    if [[ -f /usr/lib/systemd/system/haveged.service ]]; then
        sudo systemctl enable --now haveged
    fi
    
    # Auditd
    if command -v auditd &>/dev/null; then
        sudo systemctl enable --now auditd
    elif command -v pacman &>/dev/null; then
        sudo pacman -S --noconfirm audit
        sudo systemctl enable --now auditd
    fi
    
    log "Services enabled"
}

# --- Tmux Config ---
setup_tmux() {
    log "Setting up tmux..."
    mkdir -p ~/.config/tmux
    
    if [[ -f "$MIOZU_DIR/.config/tmux/tmux.conf" ]]; then
        ln -sf "$MIOZU_DIR/.config/tmux/tmux.conf" ~/.config/tmux/tmux.conf
        log "Tmux config linked"
    else
        warn "No tmux config in miozu, skipping"
    fi
}

# --- File Integrity ---
setup_integrity() {
    log "Setting up file integrity monitoring..."
    
    sudo tee /usr/local/bin/integrity-check > /dev/null << 'INTEOF'
#!/bin/bash
CHECKSUM_FILE="/var/lib/integrity/checksums.sha256"
REPORT_FILE="/var/log/integrity-report.log"
mkdir -p /var/lib/integrity

FILES=(
    /etc/passwd
    /etc/shadow
    /etc/group
    /etc/sudoers
    /etc/ssh/sshd_config
    /etc/nftables.conf
)

if [ "$1" == "init" ]; then
    rm -f "$CHECKSUM_FILE"
    for f in "${FILES[@]}"; do
        [ -f "$f" ] && sha256sum "$f" >> "$CHECKSUM_FILE"
    done
    echo "Integrity database initialized"
    exit 0
fi

[ ! -f "$CHECKSUM_FILE" ] && echo "Run with 'init' first" && exit 1

CHANGES=$(sha256sum -c "$CHECKSUM_FILE" 2>&1 | grep -v ": OK")
if [ -n "$CHANGES" ]; then
    echo "$(date): INTEGRITY ALERT" >> "$REPORT_FILE"
    echo "$CHANGES" >> "$REPORT_FILE"
    echo "CHANGES DETECTED:"
    echo "$CHANGES"
    exit 1
else
    echo "All files OK"
    exit 0
fi
INTEOF
    
    sudo chmod +x /usr/local/bin/integrity-check
    sudo /usr/local/bin/integrity-check init
    
    # Cron job
    echo "0 3 * * * root /usr/local/bin/integrity-check >> /var/log/integrity-report.log 2>&1" | \
        sudo tee /etc/cron.d/integrity-check > /dev/null
    
    log "Integrity monitoring configured"
}

# --- Main ---
main() {
    echo -e "${YELLOW}╔══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║           Miozu Server Installation                      ║${NC}"
    echo -e "${YELLOW}╚══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Log: $LOG_FILE"
    echo ""
    
    preflight_checks
    backup_configs
    install_paru
    install_packages
    
    echo ""
    read -p "Configure SSH + Firewall hardening? [Y/n]: " reply
    if [[ ! $reply =~ ^[Nn]$ ]]; then
        setup_ssh
        setup_firewall
    fi
    
    read -p "Configure CrowdSec? [Y/n]: " reply
    [[ ! $reply =~ ^[Nn]$ ]] && setup_crowdsec
    
    read -p "Apply kernel hardening? [Y/n]: " reply
    [[ ! $reply =~ ^[Nn]$ ]] && setup_kernel_hardening
    
    setup_docker
    setup_services
    setup_tmux
    setup_integrity
    
    echo ""
    echo -e "${GREEN}╔══════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║           Installation Complete!                         ║${NC}"
    echo -e "${GREEN}╚══════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Backup location: $BACKUP_DIR"
    echo ""
    echo "Next steps:"
    echo "  1. Re-login for docker group: exit && ssh -p $CONFIGURED_SSH_PORT $(whoami)@$SERVER_IP"
    echo "  2. Start tmux session: tmux new -s main"
    echo ""
}

main "$@" 2>&1 | tee -a "$LOG_FILE"
