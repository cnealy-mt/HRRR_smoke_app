
#--------------------------Create Hourly Archive-------------------------

# Set paths
archive_path <- "data//archive//county_hourly_archive.rds"

# Ensure the archive folder exists
if (!dir.exists("data//archive")) {
  dir.create("data//archive", recursive = TRUE)
}

# Prepare data
filter_date <- as.Date(update_date) - 1
hourly <- readRDS(paste0("data//county_hrly_avg//", compare_date, "_county_hrly_avg.rds")) %>%
  rename(date_mdt = timestamp_MDT) %>%
  mutate(date = as.Date(date_mdt)) %>%
  filter(date == filter_date)

# Load or create archive
if (file.exists(archive_path)) {
  # Load existing archive and append new data
  archive_data <- readRDS(archive_path)
  archive_data <- bind_rows(archive_data, hourly) %>% # if/when new variable columns are added, this section will keep duplicate rows with the new variable data
    mutate(na_count = rowSums(is.na(.))) %>%
    group_by(county, fcst_hour, date_mdt) %>%
    slice_min(order_by = na_count, with_ties = FALSE) %>%
    ungroup() %>%
    select(-na_count)
} else {
  # Create new archive
  archive_data <- hourly
}

# Define date window
start_date <- as.Date(update_date) - lubridate::years(5)
end_date <- as.Date(update_date)

# Filter archive_data within date range
archive_data_filtered <- archive_data %>%
  filter(date >= start_date & date <= end_date)

# Save updated archive
saveRDS(archive_data_filtered, archive_path)


#--------------------------Create Daily Archive-------------------------

# Set paths
archive_path <- "data//archive//county_24hr_archive.rds"

# Prepare data
daily <- readRDS(paste0("data//county_24hr_avg//", compare_date, "_county_24hr_avg.rds")) 

# Load or create archive
if (file.exists(archive_path)) {
  # Load existing archive and append new data
  archive_data <- readRDS(archive_path)
  archive_data <- bind_rows(archive_data, daily) %>% # if/when new variable columns are added, this section will keep duplicate rows with the new variable data
    mutate(na_count = rowSums(is.na(.))) %>%
    group_by(county, date) %>%
    slice_min(order_by = na_count, with_ties = FALSE) %>%
    ungroup() %>%
    select(-na_count)
} else {
  # Create new archive
  archive_data <- daily
}

# Define date window
start_date <- as.Date(update_date) - lubridate::years(5)
end_date <- as.Date(update_date)

# Filter archive_data within date range
archive_data_filtered <- archive_data %>%
  filter(date >= start_date & date <= end_date)

# Save updated archive
saveRDS(archive_data_filtered, archive_path)




