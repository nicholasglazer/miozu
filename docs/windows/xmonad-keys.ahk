; XMonad Key Passthrough for WSL2/VcXsrv
; Blocks Windows key from opening Start Menu when VcXsrv is focused
;
; Installation:
; 1. Install AutoHotkey v2: winget install AutoHotkey.AutoHotkey
; 2. Double-click this script to run it
; 3. For auto-start: Win+R → shell:startup → copy this file there
;
; Behavior:
; - When VcXsrv focused: Win key blocked (no Start Menu)
; - Alt+Tab: always works with Windows
; - Alt+Shift: switches keyboard layout

#Requires AutoHotkey v2.0
#SingleInstance Force

; Check if VcXsrv is active
IsVcXsrvActive() {
    return WinActive("ahk_exe vcxsrv.exe")
}

; When VcXsrv is active, block Win key from opening Start Menu
#HotIf IsVcXsrvActive()

; Block Win key alone (prevents Start Menu popup)
LWin::return
RWin::return

; Block Win+key combinations (let them pass to VcXsrv)
; Letters
#a::return
#b::return
#c::return
#d::return
#e::return
#f::return
#g::return
#h::return
#i::return
#j::return
#k::return
#l::return
#m::return
#n::return
#o::return
#p::return
#q::return
#r::return
#s::return
#t::return
#u::return
#v::return
#w::return
#x::return
#y::return
#z::return

; Numbers
#1::return
#2::return
#3::return
#4::return
#5::return
#6::return
#7::return
#8::return
#9::return
#0::return

; Special keys
#Enter::return
#Space::return
#Tab::return
#Backspace::return
#Escape::return

; Win+Shift combinations
#+a::return
#+b::return
#+c::return
#+d::return
#+e::return
#+f::return
#+g::return
#+h::return
#+i::return
#+j::return
#+k::return
#+l::return
#+m::return
#+n::return
#+o::return
#+p::return
#+q::return
#+r::return
#+s::return
#+t::return
#+u::return
#+v::return
#+w::return
#+x::return
#+y::return
#+z::return

; Win+Shift+numbers
#+1::return
#+2::return
#+3::return
#+4::return
#+5::return
#+6::return
#+7::return
#+8::return
#+9::return
#+0::return

; Win+Shift special
#+Enter::return
#+Space::return

#HotIf

; Tray notification
TrayTip("XMonad Keys", "Win key blocked when VcXsrv focused", 1)

; Tray menu
A_TrayMenu.Delete()
A_TrayMenu.Add("About", AboutBox)
A_TrayMenu.Add()
A_TrayMenu.Add("Exit", (*) => ExitApp())
A_TrayMenu.Default := "About"
TraySetIcon("shell32.dll", 173)

AboutBox(*) {
    MsgBox("XMonad Key Passthrough`n`nWhen VcXsrv is focused:`n• Win key blocked`n• Win+key combos blocked`n`nAlways works:`n• Alt+Tab (Windows)`n• Alt+Shift (language)", "XMonad Keys")
}
