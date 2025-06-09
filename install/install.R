cat("Starting HRRR Smoke App installation...\n")
# --- Step: Restore packages from renv.lock ---
cat("Restoring R package environment using renv.lock...\n")

# Make sure renv is installed before using it
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restore packages as defined in renv.lock
renv::restore(prompt = FALSE)

cat("✅ Package environment restored.\n")

# --- Step 1: Detect Paths ---
app_dir <- normalizePath(".")
task_dir <- file.path(app_dir, "tasks")
if (!dir.exists(task_dir)) dir.create(task_dir)

rscript_path <- file.path(R.home("bin"), "Rscript.exe")
git_path <- Sys.which("git")

if (!file.exists(rscript_path)) stop("Could not find Rscript.exe.")
if (git_path == "") stop("Could not find git.exe. Make sure Git is installed and in your system PATH.")

cat("Rscript path: ", rscript_path, "\n")
cat("Git path: ", git_path, "\n")
cat("App directory: ", app_dir, "\n")
cat("Task directory: ", task_dir, "\n")

# --- Step 2: Create .bat Files ---
update_bat <- sprintf('@echo off
cd /d "%s"
"%s" "UPDATE_HRRR_APP.R" > "%s" 2>&1',
                      app_dir, rscript_path, file.path(task_dir, "HRRR_APP_UPDATE_log.txt"))

writeLines(update_bat, file.path(task_dir, "HRRR_APP_UPDATE.bat"))
cat("Created: HRRR_APP_UPDATE.bat\n")

git_pull_bat <- sprintf('@echo off
cd /d "%s"
"%s" pull > "%s" 2>&1',
                        app_dir, git_path, file.path(task_dir, "GIT_PULL_log.txt"))

writeLines(git_pull_bat, file.path(task_dir, "GIT_PULL.bat"))
cat("Created: GIT_PULL.bat\n")

# --- Step 3a: Hourly Data Update Task ---
template_path <- file.path(app_dir, "install", "utils", "HRRR_App_Update_task_template.xml")
xml_path <- file.path(task_dir, "HRRR_APP_UPDATE_task.xml")
bat_path <- file.path(task_dir, "HRRR_APP_UPDATE.bat")
task_name <- "HRRR_App_Update"

user <- Sys.getenv("USERNAME")
author <- paste0(user, "@", Sys.info()[["nodename"]])
start_time <- format(Sys.time(), "%Y-%m-%dT%H:26:00")

xml_template <- readLines(template_path, warn = FALSE)

xml_filled <- gsub("{{START_TIME}}", start_time, xml_template, fixed = TRUE)
xml_filled <- gsub("{{AUTHOR}}", author, xml_filled, fixed = TRUE)
xml_filled <- gsub("{{USER}}", user, xml_filled, fixed = TRUE)
xml_filled <- gsub("{{BAT_PATH}}", bat_path, xml_filled, fixed = TRUE)

writeLines(xml_filled, xml_path, useBytes = TRUE)

cmd <- sprintf('schtasks /Create /TN "%s" /XML "%s" /F', task_name, xml_path)
status <- shell(cmd, intern = TRUE)

cat("✅ Task created from XML\n")
cat("Scheduled task: HRRR_App_Update (hourly)\n")

# --- Step 3b: Daily Git Version Control Task ---
template_path_daily <- file.path(app_dir, "install", "utils", "HRRR_App_Version_Update_task_template.xml")
xml_path_daily <- file.path(task_dir, "HRRR_APP_VERSION_UPDATE_task.xml")
bat_path_daily <- file.path(task_dir, "GIT_PULL.bat")
task_name_daily <- "HRRR_App_Version_Update"

start_time_daily <- format(Sys.time(), "%Y-%m-%dT22:00:00")

xml_template_daily <- readLines(template_path_daily, warn = FALSE)

xml_filled_daily <- gsub("{{START_TIME}}", start_time_daily, xml_template_daily, fixed = TRUE)
xml_filled_daily <- gsub("{{AUTHOR}}", author, xml_filled_daily, fixed = TRUE)
xml_filled_daily <- gsub("{{USER}}", user, xml_filled_daily, fixed = TRUE)
xml_filled_daily <- gsub("{{BAT_PATH}}", bat_path_daily, xml_filled_daily, fixed = TRUE)

writeLines(xml_filled_daily, xml_path_daily, useBytes = TRUE)

cmd_daily <- sprintf('schtasks /Create /TN "%s" /XML "%s" /F', task_name_daily, xml_path_daily)
status_daily <- shell(cmd_daily, intern = TRUE)

cat("✅ Task created from XML\n")
cat("Scheduled task: HRRR_App_Version_Update (daily @ 22:00)\n")

cat("✅ HRRR App installation complete.\n")

# --- Step 4: Create Desktop Shortcut ---
# --- Step 4a: Create HRRR_smoke_app.bat dynamically ---
cat("Creating HRRR_smoke_app.bat launcher...\n")

launch_bat <- sprintf('@echo off
REM Auto-generated launcher for HRRR Smoke App

REM Set RScript path
set "RScriptPath=%s"

REM Change working directory to app folder
cd /d "%s"

REM Launch the Shiny app
"%s" -e "shiny::runApp(\\".\\", launch.browser = TRUE)"
',
rscript_path, app_dir, rscript_path
)

launch_bat_path <- file.path(app_dir, "app.bat")
writeLines(launch_bat, launch_bat_path)
cat("✅ Created HRRR_smoke_app.bat\n")

# --- Step 4b: Create desktop shortcut ---
cat("Creating desktop shortcut...\n")
# Define paths
bat_path <- normalizePath(file.path(app_dir, "app.bat"), winslash = "\\")
icon_path <- normalizePath(file.path(app_dir, "www", "app_icon.ico"), winslash = "\\")
shortcut_name <- "AQB HRRR Smoke App"
shortcut_file <- paste0(shortcut_name, ".lnk")

# Get user's Desktop path using PowerShell (more reliable than getenv)
desktop_dir <- shell("powershell -NoProfile -Command \"[Environment]::GetFolderPath('Desktop')\"", intern = TRUE)
desktop_dir <- normalizePath(desktop_dir, winslash = "\\", mustWork = FALSE)
shortcut_path <- file.path(desktop_dir, shortcut_file)

# Create PowerShell script to generate shortcut
ps_script <- sprintf('
$WshShell = New-Object -ComObject WScript.Shell
$Shortcut = $WshShell.CreateShortcut("%s")
$Shortcut.TargetPath = "%s"
$Shortcut.IconLocation = "%s"
$Shortcut.WorkingDirectory = "%s"
$Shortcut.WindowStyle = 1
$Shortcut.Description = "%s"
$Shortcut.Save()
', shortcut_path, bat_path, icon_path, dirname(bat_path), shortcut_name)

# Write to temp PowerShell file
ps_file <- tempfile(fileext = ".ps1")
writeLines(ps_script, ps_file, useBytes = TRUE)

# Run the script
shell(sprintf('powershell -ExecutionPolicy Bypass -File "%s"', ps_file), wait = TRUE)

cat("✅ Shortcut created on desktop (", shortcut_name, ")\n")

