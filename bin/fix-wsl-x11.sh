#!/bin/bash
# WSL2 X11 Connectivity Fix and Diagnostic Script
# This script diagnoses and fixes X11 connection issues between WSL2 and VcXsrv
#
# Based on research from:
# - https://github.com/microsoft/WSL/issues/4139
# - https://skeptric.com/wsl2-xserver/
# - https://learn.microsoft.com/en-us/windows/wsl/networking

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo -e "${CYAN}========================================"
echo "  WSL2 X11 Connectivity Diagnostic"
echo -e "========================================${NC}"
echo ""

# Check if running in WSL
if ! grep -qi microsoft /proc/version 2>/dev/null; then
    echo -e "${RED}ERROR: This script is for WSL2 only${NC}"
    exit 1
fi

echo -e "${BLUE}Step 1: Detecting WSL environment...${NC}"

# Check WSL version
if grep -qi "WSL2" /proc/version 2>/dev/null; then
    echo -e "  ${GREEN}WSL2 detected${NC}"
    WSL_VERSION=2
else
    echo -e "  ${YELLOW}WSL1 detected (WSL2 recommended)${NC}"
    WSL_VERSION=1
fi

# Check for WSLg
if [[ -d /mnt/wslg ]]; then
    echo -e "  ${YELLOW}WSLg detected - but XMonad needs VcXsrv for full WM support${NC}"
    HAS_WSLG=true
else
    echo -e "  ${GREEN}No WSLg - using VcXsrv${NC}"
    HAS_WSLG=false
fi

# Get Windows host IP
WINDOWS_IP=$(grep nameserver /etc/resolv.conf 2>/dev/null | awk '{print $2}' | head -1)
echo -e "  Windows host IP: ${CYAN}${WINDOWS_IP}${NC}"

echo ""
echo -e "${BLUE}Step 2: Checking for mirrored networking mode...${NC}"

# Check if mirrored mode might be enabled (localhost would work)
MIRRORED_MODE=false
if timeout 1 bash -c "echo > /dev/tcp/localhost/6000" 2>/dev/null; then
    echo -e "  ${GREEN}Mirrored mode appears to be enabled (localhost:6000 reachable)${NC}"
    MIRRORED_MODE=true
elif timeout 1 bash -c "echo > /dev/tcp/127.0.0.1/6000" 2>/dev/null; then
    echo -e "  ${GREEN}Mirrored mode appears to be enabled (127.0.0.1:6000 reachable)${NC}"
    MIRRORED_MODE=true
else
    echo -e "  ${YELLOW}Using traditional NAT mode (will use Windows IP)${NC}"
fi

echo ""
echo -e "${BLUE}Step 3: Testing X server connectivity...${NC}"

# Determine which DISPLAY to try
if $MIRRORED_MODE; then
    TEST_DISPLAYS=("localhost:0" "127.0.0.1:0" "${WINDOWS_IP}:0")
else
    TEST_DISPLAYS=("${WINDOWS_IP}:0" "localhost:0")
fi

WORKING_DISPLAY=""

for disp in "${TEST_DISPLAYS[@]}"; do
    echo -n "  Testing DISPLAY=$disp ... "
    export DISPLAY="$disp"

    if timeout 2 xset q &>/dev/null 2>&1; then
        echo -e "${GREEN}OK!${NC}"
        WORKING_DISPLAY="$disp"
        break
    else
        echo -e "${RED}Failed${NC}"
    fi
done

echo ""

if [[ -n "$WORKING_DISPLAY" ]]; then
    echo -e "${GREEN}========================================"
    echo "  X Server Connection Successful!"
    echo -e "========================================${NC}"
    echo ""
    echo -e "Working DISPLAY: ${CYAN}$WORKING_DISPLAY${NC}"
    echo ""

    # Update shell configs with working display
    echo -e "${BLUE}Updating shell configuration...${NC}"

    # Determine the best DISPLAY setting
    if [[ "$WORKING_DISPLAY" == "localhost:0" ]] || [[ "$WORKING_DISPLAY" == "127.0.0.1:0" ]]; then
        DISPLAY_CMD='export DISPLAY=localhost:0'
        FISH_DISPLAY_CMD='set -gx DISPLAY localhost:0'
    else
        DISPLAY_CMD='export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '\''{print $2}'\''):0'
        FISH_DISPLAY_CMD='set -gx DISPLAY (grep nameserver /etc/resolv.conf | awk '\''{print $2}'\''):0'
    fi

    # Update .bashrc
    if [[ -f "$HOME/.bashrc" ]]; then
        # Remove old X11 display settings
        sed -i '/# WSL2 X11/,/LIBGL_ALWAYS/d' "$HOME/.bashrc" 2>/dev/null || true
        sed -i '/export DISPLAY=/d' "$HOME/.bashrc" 2>/dev/null || true

        cat >> "$HOME/.bashrc" << EOF

# WSL2 X11 Display Configuration (auto-configured by fix-wsl-x11.sh)
$DISPLAY_CMD
export LIBGL_ALWAYS_INDIRECT=1
export PATH="\$HOME/.local/bin:\$PATH"
EOF
        echo -e "  ${GREEN}Updated ~/.bashrc${NC}"
    fi

    # Update fish config
    if [[ -f "$HOME/.config/fish/config.fish" ]]; then
        sed -i '/# WSL2 X11/,/LIBGL_ALWAYS/d' "$HOME/.config/fish/config.fish" 2>/dev/null || true

        cat >> "$HOME/.config/fish/config.fish" << EOF

# WSL2 X11 Display Configuration (auto-configured by fix-wsl-x11.sh)
$FISH_DISPLAY_CMD
set -gx LIBGL_ALWAYS_INDIRECT 1
if not contains \$HOME/.local/bin \$PATH
    set -gx PATH \$HOME/.local/bin \$PATH
end
EOF
        echo -e "  ${GREEN}Updated ~/.config/fish/config.fish${NC}"
    fi

    echo ""
    echo -e "${GREEN}You can now run XMonad:${NC}"
    echo -e "  ${CYAN}~/start-xmonad.sh${NC}"
    echo ""

else
    echo -e "${RED}========================================"
    echo "  X Server Connection Failed!"
    echo -e "========================================${NC}"
    echo ""
    echo -e "${YELLOW}Troubleshooting steps:${NC}"
    echo ""
    echo "1. Make sure VcXsrv is running on Windows:"
    echo "   - Double-click xmonad-fullscreen.xlaunch"
    echo "   - You should see a black fullscreen window"
    echo ""
    echo "2. Fix Windows Firewall (most common issue):"
    echo "   Copy and run the firewall fix script on Windows:"
    echo ""
    echo -e "   ${CYAN}cp ~/.miozu/docs/windows/fix-wsl-firewall.bat /mnt/c/Users/YOUR_USER/Desktop/${NC}"
    echo -e "   ${CYAN}cp ~/.miozu/docs/windows/fix-wsl-firewall.ps1 /mnt/c/Users/YOUR_USER/Desktop/${NC}"
    echo ""
    echo "   Then double-click fix-wsl-firewall.bat on Windows Desktop"
    echo ""
    echo "3. For Windows 11, try enabling mirrored networking mode:"
    echo "   Create/edit %USERPROFILE%\.wslconfig with:"
    echo ""
    echo "   [wsl2]"
    echo "   networkingMode=mirrored"
    echo ""
    echo "   Then run in PowerShell: wsl --shutdown"
    echo "   And reopen WSL"
    echo ""
    echo "4. Test raw connectivity:"
    echo "   From WSL:  nc -zv $WINDOWS_IP 6000"
    echo "   Should show: Connection to $WINDOWS_IP 6000 port [tcp/x11] succeeded!"
    echo ""
    echo "5. Verify VcXsrv is listening:"
    echo "   From Windows PowerShell: netstat -an | findstr 6000"
    echo "   Should show: TCP 0.0.0.0:6000 ... LISTENING"
    echo ""

    # Additional diagnostic info
    echo -e "${BLUE}Diagnostic Information:${NC}"
    echo "  Windows IP: $WINDOWS_IP"
    echo "  WSL IP: $(hostname -I | awk '{print $1}')"
    echo "  WSL Version: $WSL_VERSION"
    echo "  WSLg Present: $HAS_WSLG"
    echo ""

    # Test if port 6000 is reachable at all
    echo -n "  Port 6000 connectivity test: "
    if timeout 2 bash -c "echo > /dev/tcp/$WINDOWS_IP/6000" 2>/dev/null; then
        echo -e "${GREEN}Port reachable but X auth failed${NC}"
        echo "  → VcXsrv may need '-ac' flag (disable access control)"
    else
        echo -e "${RED}Port blocked (firewall issue)${NC}"
        echo "  → Run the firewall fix script on Windows"
    fi
    echo ""

    exit 1
fi
