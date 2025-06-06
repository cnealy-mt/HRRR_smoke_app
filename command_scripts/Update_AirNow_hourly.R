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
source("update_scripts//get_AirNow_data.R")

# End timing
end_time <- Sys.time()
cat("✅ Update finished at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Time difference
time_diff <- end_time - start_time
cat("⏱️ Total time elapsed:", round(as.numeric(time_diff, units = "mins"), 2), "minutes\n")

