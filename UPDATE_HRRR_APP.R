library(dplyr)
library(fs)
library(glue)
library(httr)
library(jsonlite)
library(leaflet)
library(lubridate)
library(pingr)
library(readr)
library(sf)
library(slider)
library(terra)
library(tidyr)
library(tigris)
library(viridis)


if (!pingr::is_online()) stop("No internet connection.")

#----------------------Counties for calculate_county_hourly_avg & get_AirNow_data--------------------------
mt_counties <- counties(state = "MT", cb = TRUE, year = 2022)
mt_counties <- st_transform(mt_counties, crs = "EPSG:4326")

mt_v <- vect(mt_counties)


#------------------------UPDATE SCRIPT-------------------------------
update_date <- format(Sys.Date(), "%Y-%m-%d")
compare_date <- format(Sys.Date() - 2, "%Y-%m-%d")

# Function to get the latest update date from the folder
get_latest_update_date <- function(dir_path = "data/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)
  if (length(files) == 0) return(as.Date("1900-01-01")) # Fallback for empty dir
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))
  max(dates, na.rm = TRUE)
}

#------REMEMBER, INITIALIZE FIRST------
# Function to run scheduled update
run_scheduled_update <- function() {
  # Current time in UTC
  current_utc <- as.POSIXct(Sys.time(), tz = "UTC")
  current_hour <- as.numeric(format(current_utc, "%H"))
  current_min <- as.numeric(format(current_utc, "%M"))
  
  # Latest data update available
  latest_file_date <- get_latest_update_date()
  
  cat("⏰ UTC Time:", format(current_utc, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("📂 Latest file date in county_24hr_avg:", format(latest_file_date, "%Y-%m-%d"), "\n")
  cat("📅 Today's update_date:", update_date, "\n")
  
  # Run full update if it's after 14:05 UTC and the latest update is not today
  if ((current_hour > 14 || (current_hour == 14 && current_min >= 5)) &&
      latest_file_date < as.Date(update_date)) {
    
    start_time <- Sys.time()
    cat("🚀 Starting FULL update...\n")
    
    source("update_scripts/HRRR_download.R")
    source("update_scripts/calculate_VENT_RATE.R")
    source("update_scripts/calculate_county_hourly_avg.R")
    source("update_scripts/calculate_VENT_window.R")
    source("update_scripts/archive_data.R")
    source("update_scripts/get_AirNow_data.R")
    source("update_scripts/model_performance.R")
    source("update_scripts/calculate_trends.R")
    source("update_scripts/get_fire_data.R")
    
    end_time <- Sys.time()
    cat("✅ Full update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("⏱️ Total time:", round(as.numeric(end_time - start_time, units = "mins"), 2), "minutes\n")
    
  } else {
    cat("🔄 Running AirNow-only update...\n")
    source("update_scripts/get_AirNow_data.R")
    cat("✅ AirNow-only update complete at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  }
}

# Run the update logic
run_scheduled_update()



# Delete old files in data/ folders
subdirs <- dir("data", full.names = TRUE, recursive = FALSE)

for (subdir in subdirs) {
  # Skip if it's the 'data/trend' directory
  if (basename(subdir) == "trend") next
  
  if (!dir_exists(subdir)) next  # Skip if not a directory
  
  # Get all files (not folders) in this subdirectory
  files <- dir(subdir, full.names = TRUE, recursive = FALSE)
  files <- files[file.info(files)$isdir == FALSE]
  
  if (length(files) > 8) {
    # Sort by modification time (newest first)
    files_to_keep <- files %>%
      tibble::tibble(path = ., mtime = file.info(.)$mtime) %>%
      arrange(desc(mtime)) %>%
      slice_head(n = 8) %>%
      pull(path)
    
    # Files to delete = all files not in the 5 most recent
    files_to_delete <- setdiff(files, files_to_keep)
    
    for (file_path in files_to_delete) {
      cat("🗑️ Deleting old file:", file_path, "\n")
      file.remove(file_path)
    }
  }
}

