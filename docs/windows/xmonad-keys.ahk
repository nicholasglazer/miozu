; XMonad Key Passthrough for WSL2/VcXsrv
; This AutoHotkey script intercepts Windows key combinations
; and passes them to VcXsrv so XMonad can use them
;
; Installation:
; 1. Install AutoHotkey: winget install AutoHotkey.AutoHotkey
; 2. Double-click this script to run it
; 3. For auto-start: copy to shell:startup folder
;
; Note: This script only affects VcXsrv window

#Requires AutoHotkey v2.0

; Only active when VcXsrv window is focused
#HotIf WinActive("ahk_class VcXsrv")

; Pass Windows key to VcXsrv
; The ~ prefix allows the key to pass through
~LWin::return
~RWin::return

; Common XMonad keybindings - pass Win+key combinations
; These send the key to the active window (VcXsrv)

; Window management
~LWin & Return::return  ; Open terminal
~LWin & j::return       ; Focus next window
~LWin & k::return       ; Focus previous window
~LWin & h::return       ; Shrink master
~LWin & l::return       ; Expand master
~LWin & m::return       ; Focus master
~LWin & t::return       ; Push to tiling
~LWin & c::return       ; Close window
~LWin & Space::return   ; Next layout
~LWin & n::return       ; Refresh
~LWin & Tab::return     ; Next workspace

; Workspace switching (1-9)
~LWin & 1::return
~LWin & 2::return
~LWin & 3::return
~LWin & 4::return
~LWin & 5::return
~LWin & 6::return
~LWin & 7::return
~LWin & 8::return
~LWin & 9::return

; Shift combinations (move window to workspace)
~LWin & +1::return
~LWin & +2::return
~LWin & +3::return
~LWin & +4::return
~LWin & +5::return
~LWin & +6::return
~LWin & +7::return
~LWin & +8::return
~LWin & +9::return

; Quit/Restart XMonad
~LWin & +q::return      ; Quit XMonad
~LWin & q::return       ; Restart XMonad

; Application launchers
~LWin & p::return       ; dmenu/rofi
~LWin & d::return       ; Alternative launcher

#HotIf

; Show tray icon to indicate script is running
; Right-click to exit
A_TrayMenu.Add("Exit", (*) => ExitApp())
TraySetIcon("shell32.dll", 44)  ; Keyboard icon
