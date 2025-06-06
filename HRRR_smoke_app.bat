@echo off
REM Set RScript path
set "RScriptPath=C:\Users\cba573\AppData\Local\Programs\R\R-4.4.1\bin\x64\Rscript.exe"

REM Change working directory to project folder
cd /d "G:\AQ\AQPlanning\Modeling\R\R_Projects\shiny\HRRR_tools - 20250605 GDrive test copy"

REM Launch the app
"%RScriptPath%" -e "shiny::runApp('.', launch.browser = TRUE)"
