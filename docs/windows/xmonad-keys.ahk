; XMonad Key Passthrough for WSL2/VcXsrv
; Remaps Windows key to work with XMonad when VcXsrv is focused
;
; Installation:
; 1. Install AutoHotkey v2: winget install AutoHotkey.AutoHotkey
; 2. Double-click this script to run it
; 3. For auto-start: Win+R → shell:startup → copy this file there
;
; Behavior:
; - When VcXsrv is focused: Win key works as Super/Mod key in XMonad
; - When other windows are focused: Win key works normally
; - Alt+Tab always works with Windows
; - Alt+Shift switches keyboard layout (works in both)

#Requires AutoHotkey v2.0
#SingleInstance Force

; Get VcXsrv window class - it can be "VcXsrv/XWin" or just "vcxsrv"
IsVcXsrvActive() {
    return WinActive("ahk_exe vcxsrv.exe")
}

; When VcXsrv is active, intercept Win key
#HotIf IsVcXsrvActive()

; Simply suppress Win key alone (prevents Start Menu)
; Win+other keys still work and pass through to VcXsrv
LWin::return
RWin::return

; Win+Enter (terminal in XMonad)
LWin & Enter::return
RWin & Enter::return

; Win+letter combinations - let them pass through
LWin & a::return
LWin & b::return
LWin & c::return
LWin & d::return
LWin & e::return
LWin & f::return
LWin & g::return
LWin & h::return
LWin & i::return
LWin & j::return
LWin & k::return
LWin & l::return
LWin & m::return
LWin & n::return
LWin & o::return
LWin & p::return
LWin & q::return
LWin & r::return
LWin & s::return
LWin & t::return
LWin & u::return
LWin & v::return
LWin & w::return
LWin & x::return
LWin & y::return
LWin & z::return

; Win+numbers (workspace switching)
LWin & 1::return
LWin & 2::return
LWin & 3::return
LWin & 4::return
LWin & 5::return
LWin & 6::return
LWin & 7::return
LWin & 8::return
LWin & 9::return
LWin & 0::return

; Win+Space, Win+Tab
LWin & Space::return
LWin & Tab::return

; Win+Shift combinations
LWin & +Enter::return
LWin & +c::return
LWin & +q::return
LWin & +1::return
LWin & +2::return
LWin & +3::return
LWin & +4::return
LWin & +5::return
LWin & +6::return
LWin & +7::return
LWin & +8::return
LWin & +9::return

#HotIf

; Show tray notification
TrayTip("XMonad Keys", "Active - Win key blocked when VcXsrv focused", 1)

; Tray menu
A_TrayMenu.Delete()
A_TrayMenu.Add("About", AboutBox)
A_TrayMenu.Add()
A_TrayMenu.Add("Exit", (*) => ExitApp())
A_TrayMenu.Default := "About"
TraySetIcon("shell32.dll", 173)

AboutBox(*) {
    MsgBox("XMonad Key Passthrough`n`nWhen VcXsrv is focused:`n• Win key blocked (no Start Menu)`n• Win+key combos work in XMonad`n`nAlways works:`n• Alt+Tab (Windows)`n• Alt+Shift (language switch)", "XMonad Keys")
}
