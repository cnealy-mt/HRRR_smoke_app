@echo off
REM Set RScript path
set "RScriptPath=C:\Users\cba573\AppData\Local\Programs\R\R-4.4.1\bin\x64\Rscript.exe"

REM Change working directory to project folder
cd /d "C:\R Projects (dev)\GitHub\HRRR_smoke_app"

REM Launch the app
"%RScriptPath%" -e "shiny::runApp('.', launch.browser = TRUE)"
