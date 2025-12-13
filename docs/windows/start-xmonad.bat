@echo off
REM Start XMonad Session - VcXsrv + WSL2
REM Copy this to your Windows Desktop

echo Starting VcXsrv...
start "" "C:\Program Files\VcXsrv\vcxsrv.exe" -fullscreen -ac -noreset -clipboard -primary

echo Waiting for X server...
timeout /t 3 /nobreak > nul

echo Starting XMonad in WSL...
wsl -d archlinux -e bash -lc "export DISPLAY=$(grep nameserver /etc/resolv.conf | awk '{print $2}'):0; export LIBGL_ALWAYS_SOFTWARE=1; xmonad"

echo Session ended.
pause
