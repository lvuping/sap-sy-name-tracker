@echo off
REM =============================================================================
REM SAP ABAP SY-UNAME Tracker - Deployment Script (Windows)
REM =============================================================================

echo =========================================
echo SAP ABAP SY-UNAME Tracker Deployment
echo =========================================

REM Check Python installation
python --version >nul 2>&1
if %errorlevel% neq 0 (
    echo Error: Python is not installed
    echo Please install Python 3 and try again
    pause
    exit /b 1
)

REM Create output directory
if not exist "deploy\output" mkdir deploy\output

REM Run deployment generator
echo.
echo Generating deployment package...
python deploy\deploy_to_sap.py

REM Check if successful
if %errorlevel% equ 0 (
    echo.
    echo =========================================
    echo Deployment package ready!
    echo =========================================
    echo.
    echo Manual deployment steps:
    echo 1. Log into SAP GUI
    echo 2. Go to SE80 and create package Z_SYUNAME_TRACKER
    echo 3. Import the generated XML file using SAPLink ^(ZSAPLINK^)
    echo    OR
    echo    Create each object manually using SE80/SE38
    echo.
    echo Files are in: deploy\output\
    dir deploy\output\
) else (
    echo Error: Deployment generation failed
    pause
    exit /b 1
)

pause