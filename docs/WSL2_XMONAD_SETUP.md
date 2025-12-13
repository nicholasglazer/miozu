# Miozu on WSL2 + VcXsrv

Run XMonad on Windows via WSL2 Arch Linux.

---

## Prerequisites

- Windows 10/11 with WSL2
- Arch Linux installed in WSL2
- Regular user created (not root)

---

## Part 1: Arch Linux Setup (in WSL2)

### 1.1 First-time setup as root

Open WSL and run as root:

    pacman -S --needed base-devel sudo man-db man-pages wget curl git openssh

Create your user:

    useradd -m -G wheel -s /bin/bash ng
    passwd ng
    echo '%wheel ALL=(ALL:ALL) ALL' >> /etc/sudoers

### 1.2 Set default user

In Windows PowerShell (not WSL):

    Arch config --default-user ng
    wsl --shutdown

Reopen WSL - you're now logged in as your user.

### 1.3 Install paru (AUR helper)

    git clone https://aur.archlinux.org/paru.git /tmp/paru
    cd /tmp/paru
    makepkg -si

### 1.4 Clone and run Miozu installer

    git clone https://github.com/nicholasglazer/miozu.git ~/.miozu
    cd ~/.miozu
    ./bin/install-wsl.sh

This installs XMonad, Fish, fonts, and configures everything.

---

## Part 2: Windows Setup

### 2.1 Install VcXsrv

Download and install from: https://sourceforge.net/projects/vcxsrv/

Or via PowerShell:

    winget install marha.VcXsrv

### 2.2 Copy launcher to Desktop

In WSL, run (replace YOUR_USERNAME with your Windows username):

    cp ~/.miozu/docs/windows/xmonad-fullscreen.xlaunch /mnt/c/Users/YOUR_USERNAME/Desktop/

### 2.3 Add firewall rule

In PowerShell as Administrator:

    New-NetFirewallRule -DisplayName "VcXsrv" -Direction Inbound -Program "C:\Program Files\VcXsrv\vcxsrv.exe" -Action Allow

---

## Part 3: Launch XMonad

### 3.1 Start VcXsrv

Double-click `xmonad-fullscreen.xlaunch` on your Desktop.

You should see a black/gray screen with an X cursor. This is correct - the X server is waiting for clients.

### 3.2 Start XMonad

In WSL terminal:

    export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
    xmonad

NOTE: Don't use `startx` - that tries to start a local X server. We want to connect to VcXsrv.

XMonad should now appear. Press Mod+Shift+Enter to open a terminal.

### 3.3 Exit

- In XMonad: Mod+Shift+Q
- Emergency: Ctrl+Alt+Backspace
- Or just close VcXsrv from Windows taskbar

---

## Part 4: Daily Usage

### One-click launcher (optional)

Copy the batch file to Desktop:

    cp ~/.miozu/docs/windows/start-xmonad.bat /mnt/c/Users/YOUR_USERNAME/Desktop/

Edit the file in Windows and replace `Arch` with your WSL distro name if different.

Double-click to start both VcXsrv and XMonad.

### If VcXsrv won't start

Error: "Cannot establish any listening sockets - Make sure an X server isn't already running"

Fix: Kill existing VcXsrv process first.

In PowerShell:

    taskkill /IM vcxsrv.exe /F

Or use Task Manager (Ctrl+Shift+Esc), find vcxsrv.exe, End Task.

### If "Cannot open display" in WSL

Check DISPLAY variable:

    echo $DISPLAY

Should show something like 172.x.x.x:0

If empty, reload shell:

    exec bash

Or manually set:

    export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0

Then run `xmonad` (not startx).

---

## Part 5: Keybindings

### The Super/Win key problem

Windows grabs the Win key for Start menu. Options:

**Option A: Use Alt as modifier**

Edit ~/.config/xmonad/lib/Variables.hs:

    myModMask = mod1Mask  -- Alt key instead of Super

Recompile:

    xmonad --recompile && xmonad --restart

**Option B: Disable Win key in VcXsrv**

Copy AutoHotkey script to Desktop:

    cp ~/.miozu/docs/windows/disable-winkey.ahk /mnt/c/Users/YOUR_USERNAME/Desktop/

Install AutoHotkey on Windows, then run the script.

---

## Troubleshooting

### Black screen after startx

XMonad might have crashed. Check:

    cat ~/.xsession-errors

Recompile XMonad:

    xmonad --recompile

### Fonts look wrong

Rebuild font cache:

    fc-cache -fv

### XMobar not showing

Start dbus first:

    eval $(dbus-launch --sh-syntax)
    startx

---

## File Locations

WSL (Linux):
- ~/.xinitrc - X startup script
- ~/.config/xmonad/ - XMonad config
- ~/.config/fish/config.fish - Shell config with DISPLAY

Windows:
- Desktop/xmonad-fullscreen.xlaunch - VcXsrv config
- Desktop/start-xmonad.bat - One-click launcher
- Desktop/disable-winkey.ahk - AutoHotkey script
