# WSL2 X11 Debugging Guide

Comprehensive troubleshooting guide for X11 display issues between WSL2 and VcXsrv, with special attention to Windows Enterprise edition and corporate environments.

**Last Updated:** December 2024

## Table of Contents
1. [Understanding the Problem](#understanding-the-problem)
2. [Quick Checklist](#quick-checklist)
3. [Detailed Debugging Steps](#detailed-debugging-steps)
4. [Enterprise Edition Issues](#enterprise-edition-issues)
5. [Network Mode Analysis](#network-mode-analysis)
6. [Firewall Deep Dive](#firewall-deep-dive)
7. [Alternative Solutions](#alternative-solutions)
8. [Trusted Resources](#trusted-resources)

---

## Understanding the Problem

### Why WSL2 X11 is Different from WSL1
- **WSL1**: Shares the Windows network stack directly. X11 works easily with `DISPLAY=localhost:0`
- **WSL2**: Runs in a lightweight VM with its own network. Connections to Windows go through a virtual network adapter (vEthernet)

### The Root Cause
The WSL2 virtual network adapter (vEthernet) is classified as an **"Unidentified Network"** by Windows, which automatically assigns it to the **"Public" network profile**. By default, Windows Firewall blocks most incoming connections on Public networks, including port 6000 (X11).

**Reference:** [microsoft/WSL#4139](https://github.com/microsoft/WSL/issues/4139) - "Windows Defender Firewall blocks access from WSL2"

---

## Quick Checklist

Run through these checks in order:

### WSL Side (Run in WSL terminal)
```bash
# 1. Run the diagnostic script
~/.miozu/bin/fix-wsl-x11.sh

# 2. Check your DISPLAY variable
echo $DISPLAY

# 3. Test X connection manually
xset q

# 4. Test port connectivity to Windows
timeout 2 bash -c "echo > /dev/tcp/$(grep nameserver /etc/resolv.conf | awk '{print $2}')/6000" && echo "Port open" || echo "Port blocked"
```

### Windows Side (Run in PowerShell as Administrator)

```powershell
# 1. Check if VcXsrv is listening
netstat -an | findstr 6000
# Expected: TCP 0.0.0.0:6000 ... LISTENING

# 2. Check Windows edition
(Get-CimInstance -ClassName Win32_OperatingSystem).Caption
# Note: Enterprise editions may have additional restrictions

# 3. Check WSL network adapter profile
Get-NetConnectionProfile | Where-Object {$_.InterfaceAlias -like "*WSL*"}
# If NetworkCategory is "Public", firewall rules need to allow Public

# 4. List VcXsrv firewall rules
Get-NetFirewallRule | Where-Object {$_.DisplayName -like "*vcx*"} | Format-Table DisplayName,Profile,Action,Enabled

# 5. Check Hyper-V firewall (Win11 22H2+)
Get-NetFirewallHyperVRule -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" 2>$null | Format-Table Name,Direction,Action
```

---

## Detailed Debugging Steps

### Step 1: Verify VcXsrv is Running Correctly

**Critical Settings when launching XLaunch:**
1. Display number: **0** (default)
2. Display settings: "One large window" for XMonad
3. Client startup: "Start no client"
4. **IMPORTANT**: Check "Disable access control"
5. Additional parameters: `-ac` (redundant but safe)

**Verify in Windows:**
```powershell
# Check if vcxsrv.exe is running
Get-Process vcxsrv -ErrorAction SilentlyContinue

# Check listening port
netstat -an | findstr ":6000"
# Must show: TCP  0.0.0.0:6000  ...  LISTENING
# If showing 127.0.0.1:6000 only, VcXsrv needs restart with correct settings
```

### Step 2: Test Raw Network Connectivity

From WSL:
```bash
# Get Windows IP
WIN_IP=$(grep nameserver /etc/resolv.conf | awk '{print $2}')
echo "Windows IP: $WIN_IP"

# Test ping (may fail even when X11 works - ICMP often blocked)
ping -c 1 $WIN_IP

# Test TCP port 6000
nc -zv $WIN_IP 6000 2>&1

# Alternative TCP test
timeout 2 bash -c "echo > /dev/tcp/$WIN_IP/6000" && echo "SUCCESS" || echo "FAILED"
```

### Step 3: Test X11 Connection

```bash
# Set DISPLAY for NAT mode
export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0

# Test X connection
xset q

# If that fails, try with explicit error output
DISPLAY=$DISPLAY xterm -e "echo X11 works; sleep 5" 2>&1
```

**Common Errors:**
- `Can't open display`: Either firewall blocking or VcXsrv not running
- `No protocol specified`: VcXsrv missing "Disable access control"
- `BadAccess`: Another X server (WSLg) may be interfering

### Step 4: Check for WSLg Conflicts

If Windows 11 with WSLg:
```bash
# Check for WSLg socket
ls -la /tmp/.X11-unix/
ls -la /mnt/wslg/.X11-unix/ 2>/dev/null

# WSLg sets DISPLAY=:0 by default
# For VcXsrv, you need to override with the Windows IP
```

---

## Enterprise Edition Issues

Windows Enterprise editions may have additional restrictions that prevent X11 connections:

### Group Policy Overrides

**Check for applied policies:**
```cmd
# In Command Prompt (as Administrator)
gpresult /r

# Look for: "Computer Settings" → "Windows Settings" → "Security Settings" → "Windows Defender Firewall"
```

**Common Enterprise Restrictions:**
- Forced firewall profiles
- Blocked port ranges
- Disabled ability to add local firewall rules
- Third-party endpoint protection software

### Endpoint Protection Software

Enterprise environments often have additional security software:
- **Symantec/Broadcom Endpoint Protection**: [Known to block WSL2 networking](https://knowledge.broadcom.com/external/article/221329)
- **CrowdStrike Falcon**: May interfere with Hyper-V networking
- **Microsoft Defender for Endpoint**: Additional firewall rules

**Workarounds:**
1. Contact IT to whitelist VcXsrv or port 6000
2. Request exception for WSL2 vEthernet adapter
3. Use mirrored networking mode (if supported)

### Hyper-V Firewall (Windows 11 22H2+)

Starting with Windows 11 22H2 and WSL 2.0.9, there's an additional Hyper-V firewall layer:

```powershell
# Check Hyper-V firewall rules for WSL
Get-NetFirewallHyperVRule -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}"

# Allow all inbound traffic from WSL
Set-NetFirewallHyperVVMSetting -Name "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" -DefaultInboundAction Allow

# Alternative: Create specific rule for port 6000
New-NetFirewallHyperVRule -Name "WSL X11" -Direction Inbound -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" -Protocol TCP -LocalPorts 6000 -Action Allow
```

**Reference:** [Microsoft Hyper-V Firewall Documentation](https://learn.microsoft.com/en-us/windows/security/operating-system-security/network-security/windows-firewall/hyper-v-firewall)

---

## Network Mode Analysis

### NAT Mode (Default)

The traditional WSL2 networking mode where WSL2 gets its own IP address.

**How it works:**
- WSL2 has IP in `172.16.0.0/12` range
- Windows host accessible via gateway (usually in `/etc/resolv.conf`)
- Traffic goes through vEthernet adapter

**DISPLAY setting:**
```bash
export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
```

**Firewall requirements:**
- VcXsrv must allow connections from `172.16.0.0/12`, `192.168.0.0/16`, `10.0.0.0/8`
- Rules must apply to **Public** profile (or **Any**)

### Mirrored Mode (Windows 11 22H2+)

New networking mode where WSL2 shares Windows' network interfaces.

**How to enable** (in `%USERPROFILE%\.wslconfig`):
```ini
[wsl2]
networkingMode=mirrored
dnsTunneling=true
autoProxy=true
```

**Restart WSL:**
```powershell
wsl --shutdown
```

**DISPLAY setting:**
```bash
export DISPLAY=localhost:0
```

**Advantages:**
- Simpler configuration
- Better VPN compatibility
- localhost works directly

**Limitations:**
- Requires Windows 11 22H2 or later
- May not work with some corporate security software
- Hyper-V firewall still applies

**Reference:** [Microsoft WSL Networking](https://learn.microsoft.com/en-us/windows/wsl/networking)

---

## Firewall Deep Dive

### Understanding Windows Firewall Profiles

| Profile | Description | Default Behavior |
|---------|-------------|------------------|
| **Domain** | Connected to corporate domain | Moderate restrictions |
| **Private** | Home/work trusted network | Permissive |
| **Public** | Untrusted (airports, cafes) | Most restrictive |

**WSL2's vEthernet adapter is classified as "Unidentified Network" = Public profile**

### Creating Proper Firewall Rules

```powershell
# Remove existing VcXsrv rules (start fresh)
Get-NetFirewallRule | Where-Object {$_.DisplayName -like "*vcx*"} | Remove-NetFirewallRule

# Create program-based rule (most secure)
New-NetFirewallRule `
    -DisplayName "VcXsrv WSL2" `
    -Direction Inbound `
    -Program "C:\Program Files\VcXsrv\vcxsrv.exe" `
    -Action Allow `
    -Profile Any `
    -RemoteAddress 172.16.0.0/12,192.168.0.0/16,10.0.0.0/8 `
    -Protocol TCP `
    -Enabled True

# Create port-based rule (fallback)
New-NetFirewallRule `
    -DisplayName "WSL2 X11 Port 6000" `
    -Direction Inbound `
    -LocalPort 6000 `
    -Protocol TCP `
    -Action Allow `
    -Profile Any `
    -RemoteAddress 172.16.0.0/12,192.168.0.0/16,10.0.0.0/8 `
    -Enabled True
```

### Temporarily Disable Firewall for Testing

**CAUTION: Only for debugging, re-enable immediately!**

```powershell
# Disable Public profile firewall
Set-NetFirewallProfile -Profile Public -Enabled False

# TEST YOUR CONNECTION HERE

# Re-enable immediately
Set-NetFirewallProfile -Profile Public -Enabled True
```

If X11 works with firewall disabled but not enabled, it's definitely a firewall rule issue.

---

## Alternative Solutions

### Option 1: Use WSLg (Windows 11)

If you don't need a full window manager, WSLg provides built-in X11 support:

```bash
# WSLg automatically sets DISPLAY=:0
# Test with a simple X application
xeyes

# For GUI apps, this works out of the box
firefox &
```

**Note:** XMonad and other window managers require VcXsrv because WSLg has its own compositor.

### Option 2: X410 (Commercial Alternative)

[X410](https://x410.dev/) is a paid X server that may have better Windows integration:
- Better DPI scaling
- VSock support (no network needed)
- More active development

### Option 3: SSH X11 Forwarding

Use SSH to forward X11 (more complex but bypasses some issues):

```bash
# From Windows with SSH installed
ssh -X username@localhost -p 2222 xterm
```

### Option 4: VNC/RDP

Run a full Linux desktop session:
```bash
# Install desktop environment and VNC
sudo pacman -S xfce4 tigervnc

# Start VNC server
vncserver :1

# Connect from Windows using VNC viewer
```

---

## Trusted Resources

### Official Microsoft Documentation
- [WSL Networking](https://learn.microsoft.com/en-us/windows/wsl/networking)
- [WSL Enterprise](https://learn.microsoft.com/en-us/windows/wsl/enterprise)
- [Hyper-V Firewall](https://learn.microsoft.com/en-us/windows/security/operating-system-security/network-security/windows-firewall/hyper-v-firewall)
- [WSL Troubleshooting](https://learn.microsoft.com/en-us/windows/wsl/troubleshooting)

### GitHub Issues (Community Solutions)
- [WSL#4139](https://github.com/microsoft/WSL/issues/4139) - Main firewall issue thread
- [WSL#6430](https://github.com/microsoft/WSL/issues/6430) - Display issues
- [WSL#7993](https://github.com/microsoft/WSL/discussions/7993) - VcXsrv discussion
- [WSL#11172](https://github.com/microsoft/WSL/issues/11172) - Mirrored mode issues
- [WSL#11698](https://github.com/microsoft/WSL/issues/11698) - GUI apps June 2024

### Third-Party Guides
- [X410 WSL2 Cookbook](https://x410.dev/cookbook/wsl/using-x410-with-wsl2/)
- [X410 Firewall Protection](https://x410.dev/cookbook/wsl/protecting-x410-public-access-for-wsl2-via-windows-defender-firewall/)
- [Skeptric WSL2 X Server](https://skeptric.com/wsl2-xserver/)

### Known Enterprise Issues
- [Broadcom/Symantec WSL2 Issue](https://knowledge.broadcom.com/external/article/221329)

---

## Diagnostic Script

Run the comprehensive diagnostic script:
```bash
~/.miozu/bin/fix-wsl-x11.sh
```

This script will:
1. Detect WSL version and networking mode
2. Test port connectivity
3. Test X11 connection
4. Identify likely root cause
5. Provide specific fix instructions

---

## Contact/Support

If none of these solutions work:
1. Run `~/.miozu/bin/fix-wsl-x11.sh` and save output
2. Run Windows diagnostics (PowerShell commands above)
3. Check with IT about enterprise restrictions
4. Open issue at https://github.com/microsoft/WSL/issues with diagnostic output
