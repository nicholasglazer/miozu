# Miozu on WSL2: Complete Setup Guide

This guide sets up XMonad + full Miozu environment on Windows via WSL2 + VcXsrv.

**Your hardware:** Intel Core Ultra 7 155H, 32GB RAM, Intel Arc iGPU

---

## Part 1: Windows Side

### 1.1 Install VcXsrv

Open **PowerShell as Administrator**:

```powershell
# Option A: Using winget (recommended)
winget install marha.VcXsrv

# Option B: Manual download
# Go to: https://sourceforge.net/projects/vcxsrv/
# Download and run installer, accept defaults
```

After install, VcXsrv lives at: `C:\Program Files\VcXsrv\`

---

### 1.2 Configure VcXsrv for XMonad

Create a config file so you don't click through the wizard every time.

**Open Notepad** and paste this:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<XLaunch
  WindowMode="Fullscreen"
  ClientMode="NoClient"
  LocalClient="False"
  Display="0"
  LocalProgram="xcalc"
  RemoteProgram="xterm"
  RemotePassword=""
  PrivateKey=""
  RemoteHost=""
  RemoteUser=""
  XDMCPHost=""
  XDMCPBroadcast="False"
  XDMCPIndirect="False"
  Clipboard="True"
  ClipboardPrimary="True"
  ExtraParams="-ac -noreset"
  Wgl="False"
  DisableAC="True"
  XDMCPTerminate="False"
/>
```

**Save as:** `%USERPROFILE%\xmonad-fullscreen.xlaunch`

(That's `C:\Users\YOUR_USERNAME\xmonad-fullscreen.xlaunch`)

**Key settings explained:**
- `WindowMode="Fullscreen"` - X server takes full screen
- `Wgl="False"` - Disable OpenGL (avoids Arc GPU issues)
- `DisableAC="True"` - Allow connections from WSL
- `-ac -noreset` - Access control off, don't reset on last client exit

---

### 1.3 Windows Firewall Rule

VcXsrv needs to accept connections from WSL2.

**PowerShell as Administrator:**

```powershell
# Allow VcXsrv through firewall for WSL
New-NetFirewallRule -DisplayName "VcXsrv for WSL2" `
  -Direction Inbound `
  -Program "C:\Program Files\VcXsrv\vcxsrv.exe" `
  -Action Allow `
  -Profile Private
```

**Or manually:**
1. Press `Win + R`, type `wf.msc`, Enter
2. Click "Inbound Rules" → "New Rule..."
3. Select "Program" → Browse to `C:\Program Files\VcXsrv\vcxsrv.exe`
4. Allow the connection
5. Check "Private" only (not Public for security)
6. Name it "VcXsrv for WSL2"

---

### 1.4 Create Desktop Shortcut

**Right-click Desktop** → New → Shortcut

**Location:**
```
"C:\Program Files\VcXsrv\vcxsrv.exe" -fullscreen -ac -noreset -wgl -clipboard -primary
```

**Name:** `XMonad Session`

**Or double-click** `xmonad-fullscreen.xlaunch` to start.

---

### 1.5 (Optional) Autostart VcXsrv on Login

Press `Win + R`, type:
```
shell:startup
```

Copy your `xmonad-fullscreen.xlaunch` file here. Now VcXsrv starts automatically.

---

### 1.6 Test VcXsrv

1. Double-click `xmonad-fullscreen.xlaunch` (or your shortcut)
2. Screen goes black/gray with X cursor - **this is correct**
3. Press `Alt + F4` or `Ctrl + Alt + Backspace` to exit

If you see the X cursor, Windows side is ready.

---

## Part 2: WSL2 Arch Linux Side

### 2.1 Get WSL2 IP Address Method

WSL2 has a virtual network. You need to tell X apps where VcXsrv is.

Add to your `~/.bashrc` or `~/.config/fish/config.fish`:

**For Fish:**
```fish
# ~/.config/fish/config.fish - add at the end

# X11 Display for VcXsrv
set -gx DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
set -gx LIBGL_ALWAYS_SOFTWARE 1
```

**For Bash:**
```bash
# ~/.bashrc - add at the end

# X11 Display for VcXsrv
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
export LIBGL_ALWAYS_SOFTWARE=1
```

---

### 2.2 Install Required Packages

```bash
# Update system
paru -Syu

# Core X11 + XMonad
paru -S --needed \
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

# Terminal + essentials
paru -S --needed \
  alacritty \
  fish \
  neovim \
  rofi \
  dunst \
  feh

# Fonts (important!)
paru -S --needed \
  ttf-roboto \
  ttf-ibm-plex \
  noto-fonts-emoji \
  ttf-nerd-fonts-symbols \
  ttf-font-awesome

# Dev tools
paru -S --needed \
  ripgrep \
  fd \
  nnn \
  ncdu \
  git \
  openssh \
  aria2 \
  zip \
  unzip
```

---

### 2.3 Install Stack (for XMonad build)

Miozu uses Stack to build XMonad:

```bash
# Install stack
curl -sSL https://get.haskellstack.org/ | sh

# Or via paru
paru -S stack
```

---

### 2.4 Build XMonad with Miozu Config

```bash
cd ~/.miozu

# Build XMonad using Miozu's script
./bin/xmonad.sh

# This creates ~/.local/bin/xmonad
# Add to PATH if not already
fish_add_path ~/.local/bin  # Fish
# or for bash: export PATH="$HOME/.local/bin:$PATH"
```

---

### 2.5 Symlink Miozu Dotfiles (WSL2-relevant only)

```bash
cd ~/.miozu

# Create directories
mkdir -p ~/.config/{xmonad,fish,doom,rofi,dunst,alacritty,fontconfig}
mkdir -p ~/.config/xmonad/lib
mkdir -p ~/.config/xmonad/xmobar

# XMonad config
ln -sf ~/.miozu/.config/xmonad/xmonad.hs ~/.config/xmonad/
ln -sf ~/.miozu/.config/xmonad/lib/*.hs ~/.config/xmonad/lib/
ln -sf ~/.miozu/.config/xmonad/xmobar/*.hs ~/.config/xmonad/xmobar/

# Fish shell
ln -sf ~/.miozu/.config/fish/config.fish ~/.config/fish/

# Rofi
ln -sf ~/.miozu/.config/rofi/miozu.rasi ~/.config/rofi/config.rasi
mkdir -p ~/.config/rofi/colors
ln -sf ~/.miozu/.config/rofi/colors/* ~/.config/rofi/colors/

# Dunst notifications
ln -sf ~/.miozu/.config/dunst/dunstrc ~/.config/dunst/

# Fonts config
ln -sf ~/.miozu/.config/fontconfig/fonts.conf ~/.config/fontconfig/

# Neovim
mkdir -p ~/.config/nvim
ln -sf ~/.miozu/.config/nvim/.vimrc ~/.config/nvim/init.vim
```

---

### 2.6 Setup Doom Emacs

```bash
# Clone Doom
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d

# Symlink Miozu Doom config BEFORE install
mkdir -p ~/.config/doom
ln -sf ~/.miozu/.config/doom/init.el ~/.config/doom/
ln -sf ~/.miozu/.config/doom/config.el ~/.config/doom/
ln -sf ~/.miozu/.config/doom/packages.el ~/.config/doom/

# Install Doom (takes a while)
~/.emacs.d/bin/doom install

# Optional: Miozu theme
mkdir -p ~/.config/doom/themes
git clone https://github.com/miozutheme/doom-miozu.git ~/.config/doom/themes/miozu

# Sync
~/.emacs.d/bin/doom sync
```

---

### 2.7 Create Startup Script

Create `~/.xinitrc`:

```bash
#!/bin/bash

# Set background color
xsetroot -solid "#1a1a2e"

# Start notification daemon
dunst &

# Start XMonad
exec xmonad
```

Make executable:
```bash
chmod +x ~/.xinitrc
```

---

### 2.8 Test XMonad

1. **On Windows:** Start VcXsrv (double-click your .xlaunch file)
2. **In WSL2 terminal:**

```bash
# Reload shell to get DISPLAY variable
exec fish  # or exec bash

# Test X connection
echo $DISPLAY  # Should show something like 172.x.x.x:0

# Quick test
xeyes  # Should show eyes following cursor

# Start XMonad
startx
```

If xeyes works but shows on wrong monitor, VcXsrv might be on a different display.

---

## Part 3: Daily Usage

### 3.1 Start Session

**Option A: Manual**
1. Double-click `XMonad Session` shortcut on Windows desktop
2. Open Windows Terminal / WSL
3. Run `startx`

**Option B: One-click from Windows**

Create `start-xmonad.bat` on Desktop:
```batch
@echo off
start "" "C:\Program Files\VcXsrv\vcxsrv.exe" -fullscreen -ac -noreset -wgl -clipboard -primary
timeout /t 2
wsl -d Arch -e bash -lc "DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 startx"
```

---

### 3.2 Exit Session

- **In XMonad:** `Mod + Shift + Q` (default quit)
- **Emergency:** `Ctrl + Alt + Backspace` kills X server
- **Back to Windows:** `Alt + Tab` or move mouse to screen edge

---

### 3.3 Switch Between Windows and XMonad

When VcXsrv is fullscreen:
- `Alt + Tab` - Switch to Windows apps
- Click VcXsrv in taskbar - Return to XMonad
- `Win` key might be grabbed by Windows - see fixes below

---

## Part 4: Keybinding Fixes

### 4.1 The Super/Win Key Problem

Windows grabs `Win` key for Start menu. Options:

**Option A: Change XMonad modMask to Alt**

Edit `~/.config/xmonad/lib/Variables.hs`:
```haskell
-- Change from:
myModMask = mod4Mask  -- Super/Win key

-- To:
myModMask = mod1Mask  -- Alt key
```

Then recompile: `xmonad --recompile && xmonad --restart`

**Option B: Use AutoHotkey on Windows**

Install AutoHotkey, create `disable-win.ahk`:
```autohotkey
#IfWinActive ahk_class vcxsrv
LWin::return
RWin::return
#IfWinActive
```

This disables Win key only when VcXsrv is focused.

**Option C: Use PowerToys**

1. Install: `winget install Microsoft.PowerToys`
2. Open PowerToys → Keyboard Manager
3. Remap `Win` to something else when VcXsrv is active

---

### 4.2 Clipboard Integration

VcXsrv handles clipboard automatically with `-clipboard -primary` flags.

Test:
```bash
# Copy in XMonad
echo "test" | xclip -selection clipboard

# Should be pasteable in Windows with Ctrl+V
```

---

## Part 5: Troubleshooting

### "Cannot open display"

```bash
# Check DISPLAY is set
echo $DISPLAY

# Should be like: 172.22.192.1:0
# If empty, add to shell config (see 2.1)

# Check VcXsrv is running on Windows
# Look for X icon in system tray
```

### "Connection refused"

```bash
# Check firewall on Windows
# Make sure VcXsrv firewall rule exists (see 1.3)

# Check VcXsrv started with -ac flag
```

### Black screen after startx

```bash
# XMonad might have crashed
# Check logs:
cat ~/.xsession-errors

# Common fix: recompile XMonad
cd ~/.config/xmonad
xmonad --recompile
```

### Fonts look bad

```bash
# Rebuild font cache
fc-cache -fv

# Check fonts installed
fc-list | grep -i "roboto"
```

### XMobar not showing

XMobar needs dbus. Check it's running:
```bash
# Start dbus if needed
eval $(dbus-launch --sh-syntax)

# Then start xmonad
```

Add to `~/.xinitrc` before `exec xmonad`:
```bash
eval $(dbus-launch --sh-syntax)
export DBUS_SESSION_BUS_ADDRESS
```

### Mouse cursor invisible

```bash
# Install cursor theme
paru -S xcursor-breeze

# Add to ~/.xinitrc before exec xmonad:
xsetroot -cursor_name left_ptr
```

---

## Part 6: Performance Tips

### 6.1 Disable WSLg (Optional)

WSLg can conflict with VcXsrv. Disable it:

**On Windows**, create/edit `%USERPROFILE%\.wslconfig`:
```ini
[wsl2]
guiApplications=false
memory=16GB
processors=8
```

Then restart WSL:
```powershell
wsl --shutdown
```

### 6.2 WSL2 Memory Limit

WSL2 can eat RAM. Limit it in `.wslconfig`:
```ini
[wsl2]
memory=16GB
swap=4GB
```

---

## Quick Reference

| Action | Command/Key |
|--------|-------------|
| Start VcXsrv | Double-click `.xlaunch` file |
| Start XMonad | `startx` in WSL |
| Exit XMonad | `Mod + Shift + Q` |
| Kill X server | `Ctrl + Alt + Backspace` |
| Switch to Windows | `Alt + Tab` |
| Recompile XMonad | `xmonad --recompile` |
| Check DISPLAY | `echo $DISPLAY` |

---

## Files Created

| File | Purpose |
|------|---------|
| `%USERPROFILE%\xmonad-fullscreen.xlaunch` | VcXsrv config |
| `~/.xinitrc` | X startup script |
| `~/.config/xmonad/*` | XMonad config (symlinked) |
| `~/.config/fish/config.fish` | Shell config with DISPLAY |

---

## Next Steps

1. Customize XMonad keybindings in `~/.config/xmonad/lib/Keybindings.hs`
2. Adjust colors in `~/.config/xmonad/lib/Variables.hs`
3. Configure XMobar in `~/.config/xmonad/xmobar/xmobar.hs`
4. Add your projects to `~/.config/xmonad/lib/Projects.hs`

Enjoy your Miozu environment on Windows!
