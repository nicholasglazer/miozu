@echo off
:: Start VcXsrv and XMonad for WSL2
:: Use display :1 because WSLg uses :0
::
:: Usage: Double-click this file
:: NOTE: Use Command Prompt, NOT PowerShell

echo ========================================
echo   Starting VcXsrv + XMonad for WSL2
echo ========================================
echo.

:: Check if VcXsrv is already running
tasklist /FI "IMAGENAME eq vcxsrv.exe" 2>NUL | find /I /N "vcxsrv.exe">NUL
if "%ERRORLEVEL%"=="0" (
    echo VcXsrv is already running. Killing existing instance...
    taskkill /F /IM vcxsrv.exe >NUL 2>&1
    timeout /t 2 /nobreak >NUL
)

:: Start VcXsrv
:: :1 = display number (use 1 because WSLg uses 0)
:: -fullscreen = fullscreen mode for XMonad
:: -clipboard = enable clipboard sharing
:: -wgl = enable OpenGL
:: -ac = disable access control (allow connections from WSL2)
:: -noreset = don't reset when last client disconnects

echo Starting VcXsrv on display :1...
if exist "C:\Program Files\VcXsrv\vcxsrv.exe" (
    start "" "C:\Program Files\VcXsrv\vcxsrv.exe" :1 -fullscreen -clipboard -wgl -ac -noreset
) else if exist "C:\Program Files (x86)\VcXsrv\vcxsrv.exe" (
    start "" "C:\Program Files (x86)\VcXsrv\vcxsrv.exe" :1 -fullscreen -clipboard -wgl -ac -noreset
) else (
    echo ERROR: VcXsrv not found!
    echo Install with: winget install marha.VcXsrv
    pause
    exit /b 1
)

:: Wait for VcXsrv to start
echo Waiting for VcXsrv to initialize...
timeout /t 3 /nobreak >NUL

:: Start XMonad in WSL
echo Starting XMonad in WSL...
echo.

:: Run start-xmonad.sh in WSL
:: Using wsl -e to execute command directly
wsl -d archlinux -e bash -c "export DISPLAY=localhost:1; export LIBGL_ALWAYS_INDIRECT=1; cd ~; ~/.local/bin/start-xmonad 2>&1 || ~/.miozu/bin/start-xmonad.sh 2>&1 || (echo 'Falling back to direct xmonad...'; xmonad)"

echo.
echo XMonad exited.
pause
