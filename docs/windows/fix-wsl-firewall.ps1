# WSL2 VcXsrv Firewall Fix Script v2.0
# Run this script as Administrator in PowerShell
#
# This script fixes the common "Cannot connect to X server" issue when using
# VcXsrv with WSL2. The problem is that Windows Firewall blocks connections
# from the WSL2 virtual network (treated as "Public" network).
#
# ENTERPRISE EDITION NOTES:
# - Group Policy may override these rules
# - Endpoint protection software may interfere
# - You may need IT assistance to whitelist VcXsrv
#
# Based on research from:
# - https://github.com/microsoft/WSL/issues/4139
# - https://learn.microsoft.com/en-us/windows/wsl/networking
# - https://learn.microsoft.com/en-us/windows/security/operating-system-security/network-security/windows-firewall/hyper-v-firewall
# - https://x410.dev/cookbook/wsl/protecting-x410-public-access-for-wsl2-via-windows-defender-firewall/

param(
    [switch]$RemoveOnly,
    [switch]$Verbose,
    [switch]$DiagnosticsOnly
)

$ErrorActionPreference = "Stop"

Write-Host "======================================================================" -ForegroundColor Cyan
Write-Host "  WSL2 VcXsrv Firewall Fix Script v2.0" -ForegroundColor Cyan
Write-Host "  Comprehensive Enterprise-Ready Configuration" -ForegroundColor Cyan
Write-Host "======================================================================" -ForegroundColor Cyan
Write-Host ""

# Check for admin privileges
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if (-not $isAdmin) {
    Write-Host "ERROR: This script must be run as Administrator!" -ForegroundColor Red
    Write-Host "Right-click PowerShell and select 'Run as Administrator'" -ForegroundColor Yellow
    exit 1
}

Write-Host "Running with Administrator privileges" -ForegroundColor Green
Write-Host ""

#############################################################################
# SECTION 1: SYSTEM INFORMATION
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 1: System Information" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

# Get Windows version info
$osInfo = Get-CimInstance -ClassName Win32_OperatingSystem
$osCaption = $osInfo.Caption
$osBuild = (Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion").CurrentBuild
$osUBR = (Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion").UBR
$osVersion = "$osBuild.$osUBR"

Write-Host "  Windows Edition: $osCaption" -ForegroundColor White
Write-Host "  Build Number:    $osVersion" -ForegroundColor White

# Check for Enterprise/Education editions
$isEnterprise = $osCaption -match "Enterprise|Education"
if ($isEnterprise) {
    Write-Host ""
    Write-Host "  WARNING: Enterprise/Education Edition Detected" -ForegroundColor Yellow
    Write-Host "  - Group Policy may override local firewall rules" -ForegroundColor Yellow
    Write-Host "  - Corporate security software may interfere" -ForegroundColor Yellow
    Write-Host "  - You may need IT assistance to whitelist VcXsrv" -ForegroundColor Yellow
}

# Check Windows 11 22H2+ for Hyper-V firewall
$hasHyperVFirewall = [int]$osBuild -ge 22621
if ($hasHyperVFirewall) {
    Write-Host ""
    Write-Host "  Windows 11 22H2+ detected - Hyper-V Firewall will be configured" -ForegroundColor Cyan
}

Write-Host ""

#############################################################################
# SECTION 2: CURRENT STATE DIAGNOSTICS
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 2: Current State Diagnostics" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

# Check VcXsrv installation
Write-Host "  Checking VcXsrv installation..." -ForegroundColor White
$vcxsrvPaths = @(
    "C:\Program Files\VcXsrv\vcxsrv.exe",
    "C:\Program Files (x86)\VcXsrv\vcxsrv.exe",
    "$env:LOCALAPPDATA\Programs\VcXsrv\vcxsrv.exe"
)

$vcxsrvPath = $null
foreach ($path in $vcxsrvPaths) {
    if (Test-Path $path) {
        $vcxsrvPath = $path
        break
    }
}

if ($vcxsrvPath) {
    Write-Host "    Found: $vcxsrvPath" -ForegroundColor Green
} else {
    Write-Host "    NOT FOUND in standard locations" -ForegroundColor Yellow
    Write-Host "    Will create port-based rules instead" -ForegroundColor Yellow
}

# Check if VcXsrv is running
$vcxsrvProcess = Get-Process vcxsrv -ErrorAction SilentlyContinue
if ($vcxsrvProcess) {
    Write-Host "    VcXsrv is running (PID: $($vcxsrvProcess.Id))" -ForegroundColor Green
} else {
    Write-Host "    VcXsrv is NOT running" -ForegroundColor Yellow
}

# Check port 6000
Write-Host ""
Write-Host "  Checking port 6000..." -ForegroundColor White
$port6000 = netstat -an | findstr ":6000.*LISTENING"
if ($port6000) {
    Write-Host "    Port 6000 is listening:" -ForegroundColor Green
    $port6000 | ForEach-Object { Write-Host "      $_" -ForegroundColor Gray }
} else {
    Write-Host "    Port 6000 is NOT listening" -ForegroundColor Red
    Write-Host "    Make sure VcXsrv is running!" -ForegroundColor Yellow
}

# Check WSL network adapter
Write-Host ""
Write-Host "  Checking WSL network adapter..." -ForegroundColor White
$wslAdapter = Get-NetAdapter | Where-Object { $_.Name -like "*WSL*" } -ErrorAction SilentlyContinue
if ($wslAdapter) {
    Write-Host "    Found: $($wslAdapter.Name) ($($wslAdapter.Status))" -ForegroundColor Green

    # Check network profile
    $wslProfile = Get-NetConnectionProfile -InterfaceAlias $wslAdapter.Name -ErrorAction SilentlyContinue
    if ($wslProfile) {
        $profileColor = if ($wslProfile.NetworkCategory -eq "Public") { "Yellow" } else { "Green" }
        Write-Host "    Network Category: $($wslProfile.NetworkCategory)" -ForegroundColor $profileColor
        if ($wslProfile.NetworkCategory -eq "Public") {
            Write-Host "    (Public = most restrictive firewall rules apply)" -ForegroundColor Yellow
        }
    }
} else {
    Write-Host "    WSL adapter not found (WSL may not be running)" -ForegroundColor Yellow
}

# Check existing firewall rules
Write-Host ""
Write-Host "  Existing VcXsrv/X11 firewall rules:" -ForegroundColor White
$existingRules = Get-NetFirewallRule | Where-Object {
    $_.DisplayName -like "*vcx*" -or
    $_.DisplayName -like "*X11*" -or
    $_.DisplayName -like "*WSL*X*" -or
    $_.DisplayName -like "*Miozu*"
} -ErrorAction SilentlyContinue

if ($existingRules) {
    foreach ($rule in $existingRules) {
        $color = if ($rule.Action -eq "Allow" -and $rule.Enabled) { "Green" } else { "Yellow" }
        Write-Host "    - $($rule.DisplayName) [Profile: $($rule.Profile), Action: $($rule.Action), Enabled: $($rule.Enabled)]" -ForegroundColor $color
    }
} else {
    Write-Host "    None found" -ForegroundColor Yellow
}

# Check Hyper-V firewall (Win11 22H2+)
if ($hasHyperVFirewall) {
    Write-Host ""
    Write-Host "  Hyper-V Firewall Rules for WSL:" -ForegroundColor White
    try {
        $hyperVRules = Get-NetFirewallHyperVRule -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" -ErrorAction SilentlyContinue
        if ($hyperVRules) {
            foreach ($rule in $hyperVRules) {
                $color = if ($rule.Action -eq "Allow") { "Green" } else { "Red" }
                Write-Host "    - $($rule.Name) [Direction: $($rule.Direction), Action: $($rule.Action)]" -ForegroundColor $color
            }
        } else {
            Write-Host "    No specific rules found" -ForegroundColor Yellow
        }

        # Check default action
        $hyperVSettings = Get-NetFirewallHyperVVMSetting -Name "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" -ErrorAction SilentlyContinue
        if ($hyperVSettings) {
            $actionColor = if ($hyperVSettings.DefaultInboundAction -eq "Allow") { "Green" } else { "Red" }
            Write-Host "    Default Inbound Action: $($hyperVSettings.DefaultInboundAction)" -ForegroundColor $actionColor
        }
    } catch {
        Write-Host "    Could not query Hyper-V firewall (may not be enabled)" -ForegroundColor Gray
    }
}

# Check for Group Policy firewall restrictions (Enterprise)
if ($isEnterprise) {
    Write-Host ""
    Write-Host "  Checking for Group Policy firewall restrictions..." -ForegroundColor White

    $gpFirewall = Get-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\WindowsFirewall\DomainProfile" -ErrorAction SilentlyContinue
    if ($gpFirewall) {
        Write-Host "    Domain Profile GP Settings detected" -ForegroundColor Yellow
        Write-Host "    (Local rules may be overridden)" -ForegroundColor Yellow
    }

    $gpFirewall = Get-ItemProperty -Path "HKLM:\SOFTWARE\Policies\Microsoft\WindowsFirewall\PublicProfile" -ErrorAction SilentlyContinue
    if ($gpFirewall) {
        Write-Host "    Public Profile GP Settings detected" -ForegroundColor Yellow
        Write-Host "    (Local rules may be overridden)" -ForegroundColor Yellow
    }
}

Write-Host ""

if ($DiagnosticsOnly) {
    Write-Host "Diagnostics complete. Use -DiagnosticsOnly:$false to apply fixes." -ForegroundColor Cyan
    exit 0
}

#############################################################################
# SECTION 3: REMOVE OLD RULES
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 3: Removing Old/Conflicting Rules" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

$rulesToRemove = @(
    "WSL2 VcXsrv",
    "WSL2 X11",
    "VcXsrv WSL2",
    "X11 WSL",
    "WSL X11",
    "Miozu VcXsrv",
    "Miozu WSL2 X11",
    "WSL2 VcXsrv vEthernet"
)

foreach ($ruleName in $rulesToRemove) {
    $existing = Get-NetFirewallRule -DisplayName $ruleName -ErrorAction SilentlyContinue
    if ($existing) {
        Remove-NetFirewallRule -DisplayName $ruleName
        Write-Host "  Removed: $ruleName" -ForegroundColor Gray
    }
}

# Also remove old VcXsrv rules that might be misconfigured
$vcxsrvRules = Get-NetFirewallRule -DisplayName "*vcxsrv*" -ErrorAction SilentlyContinue
foreach ($rule in $vcxsrvRules) {
    # Check if it's blocking or only allows private
    $profile = $rule.Profile
    if ($profile -notmatch "Public" -or $rule.Action -eq "Block") {
        Write-Host "  Removing misconfigured rule: $($rule.DisplayName)" -ForegroundColor Gray
        Remove-NetFirewallRule -Name $rule.Name
    }
}

if ($RemoveOnly) {
    Write-Host ""
    Write-Host "Removal complete. Exiting without creating new rules." -ForegroundColor Green
    exit 0
}

Write-Host ""

#############################################################################
# SECTION 4: CREATE NEW FIREWALL RULES
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 4: Creating New Firewall Rules" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

# WSL2 uses IP ranges in 172.16.0.0/12 and sometimes 192.168.0.0/16
$wslSubnets = @("172.16.0.0/12", "192.168.0.0/16", "10.0.0.0/8")

# Rule 1: Allow VcXsrv program from WSL2 subnets (if VcXsrv found)
if ($vcxsrvPath) {
    try {
        New-NetFirewallRule `
            -DisplayName "Miozu VcXsrv" `
            -Description "Allow VcXsrv X server connections from WSL2 (program rule)" `
            -Direction Inbound `
            -Program $vcxsrvPath `
            -Action Allow `
            -Profile Any `
            -RemoteAddress $wslSubnets `
            -Protocol TCP `
            -Enabled True | Out-Null
        Write-Host "  Created: Miozu VcXsrv (program rule)" -ForegroundColor Green
    }
    catch {
        Write-Host "  Warning: Could not create program rule: $_" -ForegroundColor Yellow
    }
}

# Rule 2: Allow X11 port 6000 from WSL2 subnets
try {
    New-NetFirewallRule `
        -DisplayName "Miozu WSL2 X11" `
        -Description "Allow X11 connections on port 6000 from WSL2 (port rule)" `
        -Direction Inbound `
        -LocalPort 6000 `
        -Protocol TCP `
        -Action Allow `
        -Profile Any `
        -RemoteAddress $wslSubnets `
        -Enabled True | Out-Null
    Write-Host "  Created: Miozu WSL2 X11 (port 6000 rule)" -ForegroundColor Green
}
catch {
    Write-Host "  Warning: Could not create port rule: $_" -ForegroundColor Yellow
}

# Rule 3: Allow WSL vEthernet adapter specifically (if found)
if ($wslAdapter) {
    try {
        New-NetFirewallRule `
            -DisplayName "WSL2 VcXsrv vEthernet" `
            -Description "Allow VcXsrv via WSL virtual ethernet adapter" `
            -Direction Inbound `
            -InterfaceAlias $wslAdapter.Name `
            -Action Allow `
            -Profile Any `
            -Enabled True | Out-Null
        Write-Host "  Created: WSL2 VcXsrv vEthernet (adapter rule for $($wslAdapter.Name))" -ForegroundColor Green
    }
    catch {
        Write-Host "  Note: Could not create vEthernet rule: $_" -ForegroundColor Gray
    }
}

# Rule 4: Allow localhost (for mirrored mode)
try {
    New-NetFirewallRule `
        -DisplayName "Miozu X11 Localhost" `
        -Description "Allow X11 on localhost for mirrored networking mode" `
        -Direction Inbound `
        -LocalPort 6000 `
        -Protocol TCP `
        -Action Allow `
        -Profile Any `
        -LocalAddress 127.0.0.1 `
        -Enabled True | Out-Null
    Write-Host "  Created: Miozu X11 Localhost (for mirrored mode)" -ForegroundColor Green
}
catch {
    Write-Host "  Note: Could not create localhost rule (may already exist)" -ForegroundColor Gray
}

Write-Host ""

#############################################################################
# SECTION 5: CONFIGURE HYPER-V FIREWALL (Win11 22H2+)
#############################################################################
if ($hasHyperVFirewall) {
    Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
    Write-Host "  SECTION 5: Configuring Hyper-V Firewall" -ForegroundColor Blue
    Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

    try {
        # Allow all inbound for WSL2
        Set-NetFirewallHyperVVMSetting -Name '{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}' -DefaultInboundAction Allow -ErrorAction Stop
        Write-Host "  Set WSL2 Hyper-V firewall DefaultInboundAction to Allow" -ForegroundColor Green
    }
    catch {
        Write-Host "  Note: Could not configure Hyper-V firewall: $_" -ForegroundColor Yellow
        Write-Host "  This may be restricted by Group Policy" -ForegroundColor Yellow
    }

    # Also try to create specific rule
    try {
        # Remove old rule if exists
        Remove-NetFirewallHyperVRule -Name "Miozu WSL X11" -ErrorAction SilentlyContinue

        New-NetFirewallHyperVRule `
            -Name "Miozu WSL X11" `
            -DisplayName "Miozu WSL X11" `
            -Direction Inbound `
            -VMCreatorId "{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}" `
            -Protocol TCP `
            -LocalPorts 6000 `
            -Action Allow `
            -Enabled True -ErrorAction Stop | Out-Null
        Write-Host "  Created: Hyper-V firewall rule for port 6000" -ForegroundColor Green
    }
    catch {
        Write-Host "  Note: Could not create Hyper-V specific rule: $_" -ForegroundColor Gray
    }

    Write-Host ""
}

#############################################################################
# SECTION 6: MIRRORED MODE CONFIGURATION
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 6: Mirrored Mode Information" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

if ([int]$osBuild -ge 22621) {
    Write-Host "  Your Windows version supports mirrored networking mode!" -ForegroundColor Green
    Write-Host ""
    Write-Host "  To enable (edit %USERPROFILE%\.wslconfig):" -ForegroundColor White
    Write-Host "    [wsl2]" -ForegroundColor Gray
    Write-Host "    networkingMode=mirrored" -ForegroundColor Gray
    Write-Host "    dnsTunneling=true" -ForegroundColor Gray
    Write-Host "    autoProxy=true" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  Then: wsl --shutdown (and reopen WSL)" -ForegroundColor White
    Write-Host ""
    Write-Host "  With mirrored mode, use: DISPLAY=localhost:0" -ForegroundColor Cyan
} else {
    Write-Host "  Mirrored mode requires Windows 11 22H2 (build 22621+)" -ForegroundColor Yellow
    Write-Host "  Your build: $osBuild" -ForegroundColor Yellow
    Write-Host "  Using NAT mode: DISPLAY=<windows_ip>:0" -ForegroundColor White
}

Write-Host ""

#############################################################################
# SECTION 7: VERIFICATION
#############################################################################
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
Write-Host "  SECTION 7: Verification" -ForegroundColor Blue
Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

$createdRules = Get-NetFirewallRule -DisplayName "Miozu*" -ErrorAction SilentlyContinue
if ($createdRules) {
    Write-Host "  Active Miozu firewall rules:" -ForegroundColor Green
    foreach ($rule in $createdRules) {
        Write-Host "    - $($rule.DisplayName) [Enabled: $($rule.Enabled), Action: $($rule.Action)]" -ForegroundColor White
    }
} else {
    Write-Host "  WARNING: No Miozu rules were created!" -ForegroundColor Red
    Write-Host "  This might be due to Group Policy restrictions" -ForegroundColor Yellow
}

Write-Host ""

#############################################################################
# SECTION 8: ENTERPRISE TROUBLESHOOTING
#############################################################################
if ($isEnterprise) {
    Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue
    Write-Host "  SECTION 8: Enterprise Troubleshooting" -ForegroundColor Blue
    Write-Host "----------------------------------------------------------------------" -ForegroundColor Blue

    Write-Host "  Additional steps for Enterprise environments:" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  1. Check Group Policy applied rules:" -ForegroundColor White
    Write-Host "     gpresult /r (in Command Prompt as admin)" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  2. Check for endpoint protection software:" -ForegroundColor White
    Write-Host "     - Symantec/Broadcom Endpoint Protection" -ForegroundColor Gray
    Write-Host "     - CrowdStrike Falcon" -ForegroundColor Gray
    Write-Host "     - Microsoft Defender for Endpoint" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  3. Request IT to:" -ForegroundColor White
    Write-Host "     - Whitelist VcXsrv (vcxsrv.exe)" -ForegroundColor Gray
    Write-Host "     - Allow TCP port 6000 from WSL2 subnet (172.16.0.0/12)" -ForegroundColor Gray
    Write-Host "     - Allow WSL2 Hyper-V inbound traffic" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  4. Test with firewall temporarily disabled:" -ForegroundColor White
    Write-Host "     Set-NetFirewallProfile -Profile Public -Enabled `$false" -ForegroundColor Gray
    Write-Host "     (Re-enable immediately after testing!)" -ForegroundColor Red
    Write-Host ""
}

#############################################################################
# FINAL SUMMARY
#############################################################################
Write-Host "======================================================================" -ForegroundColor Cyan
Write-Host "  Setup Complete!" -ForegroundColor Green
Write-Host "======================================================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "  Next steps:" -ForegroundColor Yellow
Write-Host "  1. Restart VcXsrv (close and reopen xmonad-fullscreen.xlaunch)" -ForegroundColor White
Write-Host "  2. Make sure 'Disable access control' is checked in VcXsrv" -ForegroundColor White
Write-Host "  3. In WSL, run: ~/start-xmonad.sh" -ForegroundColor White
Write-Host ""
Write-Host "  If still not working:" -ForegroundColor Yellow
Write-Host "  - Restart WSL: wsl --shutdown (in PowerShell)" -ForegroundColor White
Write-Host "  - Run diagnostics: ~/.miozu/bin/fix-wsl-x11.sh" -ForegroundColor White
Write-Host "  - Check: ~/.miozu/docs/WSL2_X11_DEBUGGING.md" -ForegroundColor White
Write-Host ""
