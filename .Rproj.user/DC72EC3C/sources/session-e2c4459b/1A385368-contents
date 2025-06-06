@echo off
REM Set RScript path
set "RScriptPath=C:\Users\cba573\AppData\Local\Programs\R\R-4.4.1\bin\x64\Rscript.exe"

REM Change working directory to project folder
cd /d "G:\AQ\AQPlanning\Modeling\R\R_Projects\shiny\HRRR_tools - 20250605 GDrive test copy"

REM Run R script and log output
"%RScriptPath%" "UPDATE_HRRR_APP.R" > "UPDATE_HRRR_APP_log.txt" 2>&1
