@echo off
REM Set RScript path
set "RScriptPath=C:\Users\cba573\AppData\Local\Programs\R\R-4.4.1\bin\x64\Rscript.exe"

REM Change working directory to project folder
cd /d "C:\R Projects (dev)\GitHub\HRRR_smoke_app"

REM Run R script and log output
"%RScriptPath%" "UPDATE_HRRR_APP.R" > "UPDATE_HRRR_APP_log.txt" 2>&1
