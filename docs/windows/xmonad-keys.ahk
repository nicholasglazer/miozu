; XMonad Key Passthrough for WSL2/VcXsrv
; Blocks Windows key from opening Start Menu when VcXsrv is focused
;
; Based on official AutoHotkey v2 documentation:
; https://www.autohotkey.com/docs/v2/Hotkeys.htm
; https://www.autohotkey.com/docs/v2/lib/_HotIf.htm
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

; Context-sensitive hotkeys: only active when VcXsrv is focused
; Using direct WinActive() call for optimization (per official docs)
#HotIf WinActive("ahk_exe vcxsrv.exe")

; Block Win key alone (prevents Start Menu popup)
LWin::Return
RWin::Return

; Win+letter combinations
#a::Return
#b::Return
#c::Return
#d::Return
#e::Return
#f::Return
#g::Return
#h::Return
#i::Return
#j::Return
#k::Return
#l::Return
#m::Return
#n::Return
#o::Return
#p::Return
#q::Return
#r::Return
#s::Return
#t::Return
#u::Return
#v::Return
#w::Return
#x::Return
#y::Return
#z::Return

; Win+number combinations
#1::Return
#2::Return
#3::Return
#4::Return
#5::Return
#6::Return
#7::Return
#8::Return
#9::Return
#0::Return

; Win+special keys
#Enter::Return
#Space::Return
#Tab::Return
#Backspace::Return
#Escape::Return

; Win+Shift+letter combinations
#+a::Return
#+b::Return
#+c::Return
#+d::Return
#+e::Return
#+f::Return
#+g::Return
#+h::Return
#+i::Return
#+j::Return
#+k::Return
#+l::Return
#+m::Return
#+n::Return
#+o::Return
#+p::Return
#+q::Return
#+r::Return
#+s::Return
#+t::Return
#+u::Return
#+v::Return
#+w::Return
#+x::Return
#+y::Return
#+z::Return

; Win+Shift+number combinations
#+1::Return
#+2::Return
#+3::Return
#+4::Return
#+5::Return
#+6::Return
#+7::Return
#+8::Return
#+9::Return
#+0::Return

; Win+Shift+special keys
#+Enter::Return
#+Space::Return

; End context-sensitive section
#HotIf

; Tray notification on startup
TrayTip("XMonad Keys", "Win key blocked when VcXsrv focused", 1)

; Configure tray menu
A_TrayMenu.Delete()
A_TrayMenu.Add("About", AboutHandler)
A_TrayMenu.Add()
A_TrayMenu.Add("Exit", ExitHandler)
A_TrayMenu.Default := "About"
TraySetIcon("shell32.dll", 173)

; Menu handlers
AboutHandler(*) {
    MsgBox("XMonad Key Passthrough for WSL2`n`nWhen VcXsrv is focused:`n• Win key blocked (no Start Menu)`n• Win+key combos blocked`n`nAlways works:`n• Alt+Tab (Windows app switching)`n• Alt+Shift (language switch)", "XMonad Keys")
}

ExitHandler(*) {
    ExitApp()
}
