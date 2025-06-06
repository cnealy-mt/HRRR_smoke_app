#Update Data
library(glue)
library(lubridate)
library(fs)
library(readr)
library(pingr)

if (!pingr::is_online()) stop("No internet connection.")

# Get today's date in YYYY-MM-DD format
update_date <- format(Sys.Date(), "%Y-%m-%d")
compare_date <- format(Sys.Date() - 2, "%Y-%m-%d")
# update_date <- "2024-08-08"
# compare_date <- "2024-08-06"


# Start timing
start_time <- Sys.time()
cat("✅ Update started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Run update scripts
source("update_scripts//HRRR_download.R")
source("update_scripts//calculate_VENT_RATE.R")
source("update_scripts//calculate_county_hourly_avg.R")
source("update_scripts//calculate_VENT_window.R") # uses counties from calculate_county_hourly_avg

source("update_scripts//archive_data.R") # only run this script and below after initializing 3 days of data
source("update_scripts//get_AirNow_data.R")
source("update_scripts//model_performance.R") 
source("update_scripts//calculate_trends.R")
source("update_scripts/get_fire_data.R")

# End timing
end_time <- Sys.time()
cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Time difference
time_diff <- end_time - start_time
cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n")


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

