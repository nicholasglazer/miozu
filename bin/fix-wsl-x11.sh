#!/bin/bash
# WSL2 X11 Connectivity Fix and Diagnostic Script v2.0
# This script diagnoses and fixes X11 connection issues between WSL2 and VcXsrv
#
# IMPORTANT RESEARCH FINDINGS (Dec 2024):
# - WSL2 vEthernet adapter is treated as "Public" network (blocks port 6000)
# - Windows 11 22H2+ has Hyper-V Firewall that needs separate configuration
# - Enterprise editions may have Group Policy overriding local firewall rules
# - VcXsrv MUST have "Disable access control" checked AND allow Public networks
# - Mirrored mode may not work on Enterprise with certain security software
#
# Based on research from:
# - https://github.com/microsoft/WSL/issues/4139 (main firewall issue)
# - https://github.com/microsoft/WSL/issues/6430 (display issues)
# - https://learn.microsoft.com/en-us/windows/security/operating-system-security/network-security/windows-firewall/hyper-v-firewall
# - https://learn.microsoft.com/en-us/windows/wsl/networking
# - https://x410.dev/cookbook/wsl/protecting-x410-public-access-for-wsl2-via-windows-defender-firewall/

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Diagnostic results storage
declare -A DIAGNOSTICS

echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗"
echo -e "║       WSL2 X11 Connectivity Diagnostic v2.0                    ║"
echo -e "║       Comprehensive Analysis for VcXsrv/XMonad                 ║"
echo -e "╚════════════════════════════════════════════════════════════════╝${NC}"
echo ""

# Check if running in WSL
if ! grep -qi microsoft /proc/version 2>/dev/null; then
    echo -e "${RED}ERROR: This script is for WSL2 only${NC}"
    exit 1
fi

#############################################################################
# SECTION 1: WSL ENVIRONMENT DETECTION
#############################################################################
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 1: WSL Environment Detection                          │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

# Check WSL version
echo -n "  WSL Version: "
if grep -qi "WSL2" /proc/version 2>/dev/null; then
    echo -e "${GREEN}WSL2${NC}"
    WSL_VERSION=2
    DIAGNOSTICS[wsl_version]="WSL2"
else
    echo -e "${YELLOW}WSL1 (WSL2 recommended for this setup)${NC}"
    WSL_VERSION=1
    DIAGNOSTICS[wsl_version]="WSL1"
fi

# Check for WSLg
echo -n "  WSLg Status: "
if [[ -d /mnt/wslg ]]; then
    echo -e "${YELLOW}Present (but XMonad needs VcXsrv for full WM support)${NC}"
    HAS_WSLG=true
    DIAGNOSTICS[wslg]="present"
    # Check if WSLg's X server is running
    if [[ -S /tmp/.X11-unix/X0 ]] || [[ -S /mnt/wslg/.X11-unix/X0 ]]; then
        echo -e "  ${YELLOW}  └─ WSLg X socket found - may conflict with VcXsrv${NC}"
    fi
else
    echo -e "${GREEN}Not present - Using VcXsrv${NC}"
    HAS_WSLG=false
    DIAGNOSTICS[wslg]="not_present"
fi

# Get network information
WINDOWS_IP=$(grep nameserver /etc/resolv.conf 2>/dev/null | awk '{print $2}' | head -1)
WSL_IP=$(hostname -I 2>/dev/null | awk '{print $1}')
GATEWAY_IP=$(ip route | grep default | awk '{print $3}' 2>/dev/null | head -1)

echo "  Windows Host IP (from resolv.conf): ${CYAN}${WINDOWS_IP:-NOT FOUND}${NC}"
echo "  WSL2 IP Address: ${CYAN}${WSL_IP:-NOT FOUND}${NC}"
echo "  Default Gateway: ${CYAN}${GATEWAY_IP:-NOT FOUND}${NC}"

DIAGNOSTICS[windows_ip]="$WINDOWS_IP"
DIAGNOSTICS[wsl_ip]="$WSL_IP"

#############################################################################
# SECTION 2: NETWORKING MODE DETECTION
#############################################################################
echo ""
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 2: Networking Mode Detection                          │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

# Check if mirrored mode is enabled
MIRRORED_MODE=false
MIRRORED_WORKS=false

echo -n "  Testing mirrored mode (localhost:6000): "
if timeout 1 bash -c "echo > /dev/tcp/localhost/6000" 2>/dev/null; then
    echo -e "${GREEN}Port reachable${NC}"
    MIRRORED_MODE=true
    # Test actual X connection
    if DISPLAY=localhost:0 timeout 2 xset q &>/dev/null 2>&1; then
        echo -e "    ${GREEN}└─ X connection via localhost:0 WORKS!${NC}"
        MIRRORED_WORKS=true
    else
        echo -e "    ${YELLOW}└─ Port open but X connection failed (access control?)${NC}"
    fi
elif timeout 1 bash -c "echo > /dev/tcp/127.0.0.1/6000" 2>/dev/null; then
    echo -e "${GREEN}Port reachable (127.0.0.1)${NC}"
    MIRRORED_MODE=true
else
    echo -e "${YELLOW}Not available - Using NAT mode${NC}"
fi

echo -n "  Testing NAT mode (${WINDOWS_IP}:6000): "
if [[ -n "$WINDOWS_IP" ]]; then
    if timeout 1 bash -c "echo > /dev/tcp/${WINDOWS_IP}/6000" 2>/dev/null; then
        echo -e "${GREEN}Port reachable${NC}"
        # Test actual X connection
        if DISPLAY="${WINDOWS_IP}:0" timeout 2 xset q &>/dev/null 2>&1; then
            echo -e "    ${GREEN}└─ X connection via ${WINDOWS_IP}:0 WORKS!${NC}"
            DIAGNOSTICS[nat_works]="yes"
        else
            echo -e "    ${YELLOW}└─ Port open but X connection failed (access control?)${NC}"
            DIAGNOSTICS[nat_works]="port_only"
        fi
    else
        echo -e "${RED}Port blocked (firewall issue)${NC}"
        DIAGNOSTICS[nat_works]="blocked"
    fi
else
    echo -e "${RED}No Windows IP detected${NC}"
    DIAGNOSTICS[nat_works]="no_ip"
fi

DIAGNOSTICS[mirrored_mode]="$MIRRORED_MODE"

#############################################################################
# SECTION 3: PORT 6000 DETAILED ANALYSIS
#############################################################################
echo ""
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 3: Port 6000 Connectivity Analysis                    │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

# Test connectivity methods
echo "  Testing port 6000 with different methods:"

# Method 1: TCP test
echo -n "    TCP /dev/tcp test: "
if timeout 2 bash -c "echo > /dev/tcp/${WINDOWS_IP}/6000" 2>/dev/null; then
    echo -e "${GREEN}Success${NC}"
    DIAGNOSTICS[tcp_test]="pass"
else
    echo -e "${RED}Failed${NC}"
    DIAGNOSTICS[tcp_test]="fail"
fi

# Method 2: nc (netcat) if available
if command -v nc &>/dev/null; then
    echo -n "    Netcat (nc) test: "
    if timeout 2 nc -z "$WINDOWS_IP" 6000 2>/dev/null; then
        echo -e "${GREEN}Success${NC}"
        DIAGNOSTICS[nc_test]="pass"
    else
        echo -e "${RED}Failed${NC}"
        DIAGNOSTICS[nc_test]="fail"
    fi
fi

# Method 3: ping (ICMP) to verify basic connectivity
echo -n "    Ping to Windows host: "
if ping -c 1 -W 2 "$WINDOWS_IP" &>/dev/null; then
    echo -e "${GREEN}Success${NC}"
    DIAGNOSTICS[ping_test]="pass"
else
    echo -e "${YELLOW}Failed (may be blocked by firewall - not critical)${NC}"
    DIAGNOSTICS[ping_test]="fail"
fi

#############################################################################
# SECTION 4: X SERVER CONNECTION TEST
#############################################################################
echo ""
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 4: X Server Connection Test                           │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

# Test displays in order of preference
if $MIRRORED_MODE; then
    TEST_DISPLAYS=("localhost:0" "127.0.0.1:0" "${WINDOWS_IP}:0")
else
    TEST_DISPLAYS=("${WINDOWS_IP}:0" "localhost:0")
fi

WORKING_DISPLAY=""

for disp in "${TEST_DISPLAYS[@]}"; do
    echo -n "  Testing DISPLAY=$disp ... "
    export DISPLAY="$disp"

    if timeout 3 xset q &>/dev/null 2>&1; then
        echo -e "${GREEN}✓ CONNECTED${NC}"
        WORKING_DISPLAY="$disp"
        break
    else
        # Try to determine why it failed
        if timeout 1 bash -c "echo > /dev/tcp/${disp%:0}/6000" 2>/dev/null; then
            echo -e "${YELLOW}Port open, X auth failed${NC}"
        else
            echo -e "${RED}✗ Connection refused${NC}"
        fi
    fi
done

DIAGNOSTICS[working_display]="$WORKING_DISPLAY"

#############################################################################
# SECTION 5: DIAGNOSTIC SUMMARY & ROOT CAUSE ANALYSIS
#############################################################################
echo ""
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 5: Root Cause Analysis                                │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

if [[ -n "$WORKING_DISPLAY" ]]; then
    echo -e "  ${GREEN}✓ X Server Connection Successful!${NC}"
    echo -e "  Working DISPLAY: ${CYAN}$WORKING_DISPLAY${NC}"
    DIAGNOSTICS[status]="success"
else
    echo -e "  ${RED}✗ X Server Connection Failed${NC}"
    echo ""

    # Analyze the failure
    echo -e "  ${YELLOW}Likely Causes:${NC}"

    if [[ "${DIAGNOSTICS[tcp_test]}" == "fail" ]]; then
        echo -e "  ${RED}1. FIREWALL BLOCKING PORT 6000${NC}"
        echo "     → Windows Firewall is blocking connections from WSL2"
        echo "     → The WSL2 vEthernet adapter is treated as 'Public' network"
        echo "     → VcXsrv may only allow 'Private' network connections"
        echo ""
        echo -e "  ${CYAN}Fix: Run the firewall fix script on Windows:${NC}"
        echo "     1. Copy to Windows: cp ~/.miozu/docs/windows/fix-wsl-firewall.bat /mnt/c/Users/YOUR_USER/Desktop/"
        echo "     2. Double-click fix-wsl-firewall.bat on Windows Desktop (runs as admin)"
        echo ""
    else
        echo -e "  ${YELLOW}1. PORT 6000 IS REACHABLE - X AUTH IS FAILING${NC}"
        echo "     → VcXsrv may not have 'Disable access control' checked"
        echo "     → VcXsrv may be running with wrong display number"
        echo ""
        echo -e "  ${CYAN}Fix: Restart VcXsrv with correct settings:${NC}"
        echo "     1. Close VcXsrv completely (check system tray)"
        echo "     2. Run xmonad-fullscreen.xlaunch (or XLaunch)"
        echo "     3. Select 'One large window' → 'Start no client'"
        echo "     4. IMPORTANT: Check 'Disable access control'"
        echo "     5. Finish and start"
        echo ""
    fi

    # Check for Enterprise-specific issues
    echo -e "  ${YELLOW}2. ENTERPRISE EDITION CONSIDERATIONS:${NC}"
    echo "     → Group Policy may override local firewall rules"
    echo "     → Corporate security software may block connections"
    echo "     → Hyper-V firewall (Win11 22H2+) needs separate rules"
    echo ""
    echo -e "  ${CYAN}Fix: Check with IT or use these PowerShell commands (as Admin):${NC}"
    echo "     # Check Hyper-V firewall rules"
    echo '     Get-NetFirewallHyperVRule -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}"'
    echo ""
    echo "     # Allow WSL2 inbound traffic"
    echo '     Set-NetFirewallHyperVVMSetting -Name "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" -DefaultInboundAction Allow'
    echo ""

    if [[ "$MIRRORED_MODE" == "false" ]]; then
        echo -e "  ${YELLOW}3. MIRRORED MODE NOT WORKING:${NC}"
        echo "     → Mirrored mode allows localhost:0 instead of IP"
        echo "     → Requires Windows 11 22H2 or later"
        echo "     → May not work with certain VPNs/security software"
        echo ""
        echo -e "  ${CYAN}Fix: Enable mirrored mode in Windows:${NC}"
        echo "     Create/edit %USERPROFILE%\\.wslconfig with:"
        echo "     [wsl2]"
        echo "     networkingMode=mirrored"
        echo "     Then: wsl --shutdown (in PowerShell) and reopen WSL"
        echo ""
    fi

    DIAGNOSTICS[status]="failed"
fi

#############################################################################
# SECTION 6: WINDOWS COMMANDS FOR FURTHER DEBUGGING
#############################################################################
echo ""
echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
echo -e "│  SECTION 6: Windows Commands for Further Debugging             │"
echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

echo -e "  ${MAGENTA}Run these in Windows PowerShell (as Administrator):${NC}"
echo ""
echo "  # 1. Check if VcXsrv is listening on port 6000"
echo "  netstat -an | findstr 6000"
echo "  # Should show: TCP 0.0.0.0:6000 ... LISTENING"
echo ""
echo "  # 2. Check Windows edition"
echo '  (Get-CimInstance -ClassName Win32_OperatingSystem).Caption'
echo "  # If Enterprise: Group Policy may override firewall settings"
echo ""
echo "  # 3. List all VcXsrv firewall rules"
echo '  Get-NetFirewallRule | Where-Object {$_.DisplayName -like "*vcx*" -or $_.DisplayName -like "*X11*"} | Format-Table DisplayName,Profile,Action,Enabled'
echo ""
echo "  # 4. Check if Hyper-V firewall is blocking (Win11 22H2+)"
echo '  Get-NetFirewallHyperVRule -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" | Format-Table Name,Direction,Action'
echo ""
echo "  # 5. Check network profile of WSL adapter"
echo '  Get-NetConnectionProfile | Where-Object {$_.InterfaceAlias -like "*WSL*"} | Format-Table Name,NetworkCategory'
echo "  # If 'Public': VcXsrv rules need to allow Public network"
echo ""
echo "  # 6. Temporarily disable firewall for testing (CAUTION!)"
echo '  Set-NetFirewallProfile -Profile Public -Enabled False'
echo '  # Re-enable after testing: Set-NetFirewallProfile -Profile Public -Enabled True'
echo ""

#############################################################################
# SECTION 7: UPDATE SHELL CONFIGURATION (if working)
#############################################################################
if [[ -n "$WORKING_DISPLAY" ]]; then
    echo ""
    echo -e "${BLUE}╭─────────────────────────────────────────────────────────────────╮"
    echo -e "│  SECTION 7: Updating Shell Configuration                       │"
    echo -e "╰─────────────────────────────────────────────────────────────────╯${NC}"

    # Determine the best DISPLAY setting
    if [[ "$WORKING_DISPLAY" == "localhost:0" ]] || [[ "$WORKING_DISPLAY" == "127.0.0.1:0" ]]; then
        DISPLAY_CMD='export DISPLAY=localhost:0'
        FISH_DISPLAY_CMD='set -gx DISPLAY localhost:0'
    else
        DISPLAY_CMD='export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '\''{ print $2 }'\''​):0'
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
        echo -e "  ${GREEN}✓ Updated ~/.bashrc${NC}"
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
        echo -e "  ${GREEN}✓ Updated ~/.config/fish/config.fish${NC}"
    fi

    echo ""
    echo -e "  ${GREEN}You can now run XMonad:${NC}"
    echo -e "  ${CYAN}~/start-xmonad.sh${NC}"
    echo ""
fi

#############################################################################
# SECTION 8: DIAGNOSTIC SUMMARY
#############################################################################
echo ""
echo -e "${CYAN}╔════════════════════════════════════════════════════════════════╗"
echo -e "║                    DIAGNOSTIC SUMMARY                          ║"
echo -e "╚════════════════════════════════════════════════════════════════╝${NC}"
echo ""
echo "  WSL Version:      ${DIAGNOSTICS[wsl_version]:-unknown}"
echo "  WSLg:             ${DIAGNOSTICS[wslg]:-unknown}"
echo "  Mirrored Mode:    ${DIAGNOSTICS[mirrored_mode]:-unknown}"
echo "  Windows IP:       ${DIAGNOSTICS[windows_ip]:-unknown}"
echo "  WSL IP:           ${DIAGNOSTICS[wsl_ip]:-unknown}"
echo "  TCP Port Test:    ${DIAGNOSTICS[tcp_test]:-not_run}"
echo "  Ping Test:        ${DIAGNOSTICS[ping_test]:-not_run}"
echo "  Working Display:  ${DIAGNOSTICS[working_display]:-NONE}"
echo "  Overall Status:   ${DIAGNOSTICS[status]:-unknown}"
echo ""

if [[ "${DIAGNOSTICS[status]}" == "failed" ]]; then
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}RECOMMENDED NEXT STEPS:${NC}"
    echo ""
    echo "1. Run the firewall fix script on Windows:"
    echo "   cp ~/.miozu/docs/windows/fix-wsl-firewall.bat /mnt/c/Users/YOUR_USER/Desktop/"
    echo "   Then double-click fix-wsl-firewall.bat on Windows Desktop"
    echo ""
    echo "2. Restart VcXsrv with 'Disable access control' checked"
    echo ""
    echo "3. Check if Windows Enterprise Group Policy is blocking:"
    echo "   Run: gpresult /r (in Command Prompt as admin)"
    echo ""
    echo "4. For detailed Windows debugging, see:"
    echo "   ~/.miozu/docs/WSL2_X11_DEBUGGING.md"
    echo ""
    echo "5. Trusted resources:"
    echo "   - https://github.com/microsoft/WSL/issues/4139"
    echo "   - https://learn.microsoft.com/en-us/windows/wsl/networking"
    echo "   - https://x410.dev/cookbook/wsl/protecting-x410-public-access-for-wsl2-via-windows-defender-firewall/"
    echo -e "${YELLOW}═══════════════════════════════════════════════════════════════════${NC}"
fi
