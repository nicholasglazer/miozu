@echo off
REM Start XMonad Session - VcXsrv + WSL2
REM Copy this to your Windows Desktop

echo Starting VcXsrv...
start "" "C:\Program Files\VcXsrv\vcxsrv.exe" -fullscreen -ac -noreset -clipboard -primary

echo Waiting for X server...
timeout /t 2 /nobreak > nul

echo Starting XMonad in WSL...
wsl -d Arch -e bash -lc "export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 && export LIBGL_ALWAYS_SOFTWARE=1 && startx"

echo Session ended.
pause
