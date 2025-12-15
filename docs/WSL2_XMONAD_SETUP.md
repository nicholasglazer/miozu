# Miozu on WSL2

Run XMonad on Windows via WSL2 Arch Linux with VcXsrv or WSLg.

---

## Architecture: Native vs WSL2

The Miozu installer provides the **same environment** on both native Linux and WSL2, with automatic adaptation for platform differences.

### What's Identical
- XMonad window manager and configuration
- All keybindings and workflows
- Fish shell, Neovim, Rofi, Dunst
- Doom Emacs (if installed)
- Fonts and theming
- Dotfile structure and symlinks

### What's Different (WSL Adaptations)

| Feature | Native Linux | WSL2 |
|---------|-------------|------|
| X11 Server | Native Xorg | VcXsrv/WSLg |
| Audio | PipeWire/ALSA | Windows audio |
| Bluetooth | bluez | Not available |
| Brightness | brightnessctl | Not available |
| Compositor | Picom | Not available |
| Media playback | VLC, cmus | Use Windows apps |
| Monitor hotplug | udev + systemd | Not available |
| Keyboard config | localectl | setxkbmap |
| Systemd | Always available | Optional (enable in wsl.conf) |

### Package Differences

WSL installation excludes hardware-dependent packages:
- `picom` - compositing over X forwarding is problematic
- `bluez`, `bluez-utils` - no bluetooth hardware
- `brightnessctl` - no hardware brightness control
- `alsa-*`, `pipewire-*`, `wireplumber` - no hardware audio
- `playerctl`, `cmus`, `vlc` - media apps (use Windows)
- `gstreamer` plugins - audio/video codecs

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

### 1.3 Enable systemd (recommended)

Edit `/etc/wsl.conf`:

    sudo nano /etc/wsl.conf

Add:

    [boot]
    systemd=true

    [interop]
    enabled=true
    appendWindowsPath=true

Then restart WSL:

    # In PowerShell
    wsl --shutdown

Reopen WSL. Verify systemd is running:

    systemctl --version

### 1.4 Install paru (AUR helper)

    git clone https://aur.archlinux.org/paru.git /tmp/paru
    cd /tmp/paru
    makepkg -si

### 1.5 Clone and run Miozu installer

    git clone https://github.com/nicholasglazer/miozu.git ~/.miozu
    cd ~/.miozu
    ./bin/install-wsl.sh

The installer will:
- Detect WSL2 environment automatically
- Install WSL-appropriate packages (no hardware dependencies)
- Configure X11 DISPLAY for VcXsrv/WSLg
- Set up all dotfiles via symlinks (same as native)
- Create `start-xmonad` convenience script
- Configure keyboard layout (Dvorak/QWERTY choice)

---

## Part 2: Windows Setup

### 2.1 Install VcXsrv (skip if using WSLg on Windows 11)

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

### 3.1 Start VcXsrv (skip if using WSLg)

Double-click `xmonad-fullscreen.xlaunch` on your Desktop.

You should see a black/gray screen with an X cursor. This is correct - the X server is waiting for clients.

### 3.2 Start XMonad

In WSL terminal:

    start-xmonad

Or manually:

    export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0
    xmonad

**Note**: Don't use `startx` - that tries to start a local X server. We connect to VcXsrv instead.

XMonad should now appear. Press Mod+Shift+Enter to open a terminal.

### 3.3 Exit

- In XMonad: Mod+Shift+Q
- Emergency: Ctrl+Alt+Backspace (if configured in VcXsrv)
- Or just close VcXsrv from Windows taskbar

---

## Part 4: Daily Usage

### One-click launcher (optional)

Copy the batch file to Desktop:

    cp ~/.miozu/docs/windows/start-xmonad.bat /mnt/c/Users/YOUR_USERNAME/Desktop/

Edit the file in Windows and replace `archlinux` with your WSL distro name if different.

Double-click to start both VcXsrv and XMonad.

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

    exec fish

Or manually set:

    export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0

Then run `start-xmonad` (not startx).

### Black screen after starting XMonad

XMonad might have crashed. Check:

    cat ~/.local/share/xmonad/xmonad.errors

Recompile XMonad:

    xmonad --recompile

### Fonts look wrong

Rebuild font cache:

    fc-cache -fv

### XMobar not showing

Ensure dbus is running. The `start-xmonad` script handles this, but manually:

    eval $(dbus-launch --sh-syntax)
    xmonad

### Systemd services not working

Ensure systemd is enabled in `/etc/wsl.conf`:

    [boot]
    systemd=true

Then restart WSL: `wsl --shutdown` in PowerShell.

---

## File Locations

### WSL (Linux)
- `~/.xinitrc` - X startup script
- `~/.config/xmonad/` - XMonad config
- `~/.config/fish/config.fish` - Shell config with DISPLAY
- `~/.local/bin/start-xmonad` - Convenience start script

### Windows
- `Desktop/xmonad-fullscreen.xlaunch` - VcXsrv config
- `Desktop/start-xmonad.bat` - One-click launcher
- `Desktop/disable-winkey.ahk` - AutoHotkey script

---

## WSLg (Windows 11)

If you're on Windows 11 with WSLg enabled, you don't need VcXsrv:

1. WSLg provides native GUI support
2. DISPLAY is automatically set
3. Just run `start-xmonad` directly

Check if WSLg is available:

    ls /mnt/wslg

If the directory exists, WSLg is enabled.

---

## Updating

Both native and WSL installations use the same update process:

    cd ~/.miozu
    git pull
    xmonad --recompile

For package updates:

    paru -Syu

---

## Comparison: Native vs WSL Installation Commands

| Task | Native | WSL |
|------|--------|-----|
| Install | `./bin/install.sh` | `./bin/install-wsl.sh` |
| Package list | `bin/dependencies/required-packages.txt` | `bin/dependencies/wsl-required-packages.txt` |
| Start XMonad | `startx` or display manager | `start-xmonad` |
| Audio | Works natively | Use Windows apps |
| Bluetooth | Works natively | Not available |
