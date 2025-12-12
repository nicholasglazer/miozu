; AutoHotkey script to disable Windows key when VcXsrv is focused
; This allows XMonad to use Super/Mod4 key
;
; Install AutoHotkey: winget install AutoHotkey.AutoHotkey
; Run this script, or add to startup

#IfWinActive ahk_class vcxsrv
; Disable Win key to let it pass through to XMonad
LWin::return
RWin::return

; Optional: Also disable Win+key combos that Windows intercepts
#d::return  ; Win+D (show desktop)
#e::return  ; Win+E (explorer)
#r::return  ; Win+R (run)
#l::return  ; Win+L (lock) - comment this out if you want lock to work

#IfWinActive

; Press Ctrl+Alt+Q to quit this script
^!q::ExitApp
