@echo off
:: Start VcXsrv for XMonad on WSL2
:: Use display :1 because WSLg uses :0
::
:: Usage: Double-click this file, or run from Command Prompt
:: NOTE: Use Command Prompt, NOT PowerShell (PowerShell has issues with :1 syntax)

echo Starting VcXsrv on display :1 for XMonad...
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

echo.
echo VcXsrv started on display :1
echo.
echo Now in WSL, run:
echo   ~/start-xmonad.sh
echo.
echo Press any key to close this window...
pause >NUL
