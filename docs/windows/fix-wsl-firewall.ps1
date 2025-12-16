# WSL2 VcXsrv Firewall Fix Script
# Run this script as Administrator in PowerShell
#
# This script fixes the common "Cannot connect to X server" issue when using
# VcXsrv with WSL2. The problem is that Windows Firewall blocks connections
# from the WSL2 virtual network (treated as "Public" network).
#
# Based on research from:
# - https://github.com/microsoft/WSL/issues/4139
# - https://skeptric.com/wsl2-xserver/
# - https://learn.microsoft.com/en-us/windows/wsl/networking

param(
    [switch]$RemoveOnly,
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  WSL2 VcXsrv Firewall Fix Script" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
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

# Find VcXsrv installation path
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

if (-not $vcxsrvPath) {
    Write-Host "WARNING: VcXsrv not found in standard locations" -ForegroundColor Yellow
    Write-Host "Will create port-based rules instead of program-based rules" -ForegroundColor Yellow
    Write-Host ""
}
else {
    Write-Host "Found VcXsrv at: $vcxsrvPath" -ForegroundColor Green
    Write-Host ""
}

# Remove existing WSL/VcXsrv rules to start fresh
Write-Host "Step 1: Removing existing WSL/VcXsrv firewall rules..." -ForegroundColor Yellow

$rulesToRemove = @(
    "WSL2 VcXsrv",
    "WSL2 X11",
    "VcXsrv WSL2",
    "X11 WSL",
    "WSL X11",
    "Miozu VcXsrv",
    "Miozu WSL2 X11"
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
Write-Host "Step 2: Creating new firewall rules..." -ForegroundColor Yellow

# WSL2 uses IP ranges in 172.16.0.0/12 and sometimes 192.168.0.0/16
$wslSubnets = @("172.16.0.0/12", "192.168.0.0/16", "10.0.0.0/8")

# Rule 1: Allow VcXsrv program from WSL2 subnets (if VcXsrv found)
if ($vcxsrvPath) {
    try {
        New-NetFirewallRule `
            -DisplayName "Miozu VcXsrv" `
            -Description "Allow VcXsrv X server connections from WSL2" `
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
        -Description "Allow X11 connections on port 6000 from WSL2" `
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

# Rule 3: Allow WSL vEthernet adapter specifically
try {
    $wslAdapter = Get-NetAdapter | Where-Object { $_.Name -like "*WSL*" }
    if ($wslAdapter) {
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
}
catch {
    Write-Host "  Note: Could not create vEthernet rule (adapter may not exist yet)" -ForegroundColor Gray
}

Write-Host ""
Write-Host "Step 3: Checking Windows version for mirrored mode support..." -ForegroundColor Yellow

$osVersion = [System.Environment]::OSVersion.Version
$build = (Get-ItemProperty "HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion").CurrentBuild

if ([int]$build -ge 22621) {
    Write-Host "  Windows 11 22H2+ detected (Build $build)" -ForegroundColor Green
    Write-Host "  You can use mirrored networking mode for easier setup!" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  To enable mirrored mode, add to %USERPROFILE%\.wslconfig:" -ForegroundColor White
    Write-Host "  [wsl2]" -ForegroundColor Gray
    Write-Host "  networkingMode=mirrored" -ForegroundColor Gray
    Write-Host ""
    Write-Host "  With mirrored mode, use DISPLAY=localhost:0 instead of IP address" -ForegroundColor White

    # Configure Hyper-V firewall for mirrored mode
    try {
        Set-NetFirewallHyperVVMSetting -Name '{40E0AC32-46A5-438A-A0B2-2B479E8F2E90}' -DefaultInboundAction Allow -ErrorAction SilentlyContinue
        Write-Host "  Configured Hyper-V firewall for WSL2" -ForegroundColor Green
    }
    catch {
        Write-Host "  Note: Could not configure Hyper-V firewall (may not be needed)" -ForegroundColor Gray
    }
}
else {
    Write-Host "  Windows 10 or older Windows 11 detected" -ForegroundColor Yellow
    Write-Host "  Using traditional NAT networking mode" -ForegroundColor White
}

Write-Host ""
Write-Host "Step 4: Verifying rules were created..." -ForegroundColor Yellow

$createdRules = Get-NetFirewallRule -DisplayName "Miozu*" -ErrorAction SilentlyContinue
if ($createdRules) {
    Write-Host "  Active rules:" -ForegroundColor Green
    foreach ($rule in $createdRules) {
        Write-Host "    - $($rule.DisplayName) [Enabled: $($rule.Enabled)]" -ForegroundColor White
    }
}
else {
    Write-Host "  WARNING: No rules were created!" -ForegroundColor Red
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  Setup Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "1. Restart VcXsrv (close and reopen xmonad-fullscreen.xlaunch)" -ForegroundColor White
Write-Host "2. In WSL, run: ~/start-xmonad.sh" -ForegroundColor White
Write-Host ""
Write-Host "If still not working:" -ForegroundColor Yellow
Write-Host "- Restart WSL: wsl --shutdown (in PowerShell)" -ForegroundColor White
Write-Host "- Then reopen WSL and try again" -ForegroundColor White
Write-Host ""
