# Disabling Windows Key for XMonad on WSL2

When running XMonad on WSL2 via VcXsrv, the Windows/Super key is typically captured by Windows before VcXsrv can see it. This prevents XMonad from using it as the Mod key.

## Solutions

### Option 1: AutoHotkey (Recommended)

AutoHotkey can intercept the Windows key and pass it to VcXsrv.

1. **Install AutoHotkey** on Windows:
   ```
   winget install AutoHotkey.AutoHotkey
   ```

2. **Copy the script to Windows:**
   ```bash
   cp ~/.miozu/docs/windows/xmonad-keys.ahk /mnt/c/Users/YOUR_USER/Desktop/
   ```

3. **Run the script** by double-clicking `xmonad-keys.ahk`

4. **Auto-start** (optional): Copy to `shell:startup`
   - Press `Win+R`, type `shell:startup`, press Enter
   - Copy `xmonad-keys.ahk` there

### Option 2: PowerToys Keyboard Manager

Microsoft PowerToys can remap keys system-wide.

1. Install PowerToys: `winget install Microsoft.PowerToys`
2. Open PowerToys → Keyboard Manager
3. Remap `Win` key to something else (like `F13`)
4. In XMonad config, use that key as Mod

### Option 3: Registry Edit (Disables Win key globally)

**Warning:** This disables Windows key functionality everywhere, not just in VcXsrv.

1. Open Registry Editor (`regedit`)
2. Navigate to: `HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout`
3. Create new Binary Value named `Scancode Map`
4. Set value to:
   ```
   00 00 00 00 00 00 00 00
   03 00 00 00 00 00 5B E0
   00 00 5C E0 00 00 00 00
   ```
5. Restart Windows

To undo, delete the `Scancode Map` value.

### Option 4: Use Alt as Mod Key

Modify your XMonad config to use Alt instead of Super:

```haskell
-- In ~/.config/xmonad/lib/Variables.hs
myModMask = mod1Mask  -- Alt key instead of mod4Mask (Super)
```

Then recompile XMonad:
```bash
xmonad --recompile && xmonad --restart
```

## Cursor Fix

If you see an X cursor instead of an arrow, the start-xmonad script should fix it. If not:

```bash
# In WSL, with DISPLAY set
xsetroot -cursor_name left_ptr

# Or install cursor theme
sudo pacman -S xcursor-themes
```

## Display Scaling

For 1920x1080 on a 15" laptop, the default DPI should be fine. If text is too small/large:

```bash
# Check current DPI
xdpyinfo | grep -B2 resolution

# Set custom DPI (96 is default, higher = larger)
echo "Xft.dpi: 96" | xrdb -merge
```

You can also start VcXsrv with a specific DPI:
```cmd
"C:\Program Files\VcXsrv\vcxsrv.exe" :1 -fullscreen -clipboard -wgl -ac -dpi 96
```
