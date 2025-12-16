@echo off
:: WSL2 VcXsrv Firewall Fix - Run as Administrator
:: This batch file runs the PowerShell script with elevated privileges

echo ========================================
echo   WSL2 VcXsrv Firewall Fix
echo ========================================
echo.
echo This will configure Windows Firewall to allow
echo VcXsrv connections from WSL2.
echo.
echo Press any key to continue (or Ctrl+C to cancel)...
pause >nul

:: Get the directory where this batch file is located
set "SCRIPT_DIR=%~dp0"

:: Run PowerShell script with admin privileges
powershell -Command "Start-Process powershell -ArgumentList '-ExecutionPolicy Bypass -File \"%SCRIPT_DIR%fix-wsl-firewall.ps1\"' -Verb RunAs -Wait"

echo.
echo ========================================
echo Done! Now restart VcXsrv and try again.
echo ========================================
echo.
pause
