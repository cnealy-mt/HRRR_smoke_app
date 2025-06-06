

# today <- as.Date(update_date) # old way of defining based on update_date
# start_date <- today - 2

# Get current times and offsets
current_utc_time <- now(tzone = "UTC")
current_local_time <- with_tz(current_utc_time, tzone = Sys.timezone())
local_hour <- hour(current_local_time)
utc_hour <- hour(current_utc_time)
utc_offset <- as.numeric(format(current_local_time, "%z")) / 100  # -6 for MDT, -7 for MST

# Determine cutoff hour (local midnight in UTC terms)
cutoff_hour <- ifelse(utc_offset == -6, 18, 17)

# Adjust update_date if past cutoff
adjusted_update_date <- as.Date(update_date) + ifelse(local_hour >= cutoff_hour, 1, 0)

# Format date strings for API (using adjusted date)
end_date_str <- format(adjusted_update_date, "%Y-%m-%d")
start_date_str <- format(adjusted_update_date - 3, "%Y-%m-%d")
UTC_hr <- sprintf("T%02d", utc_hour)

# Example URL construction
url <- paste0(
  "https://www.airnowapi.org/aq/data/?startDate=", start_date_str, UTC_hr,
  "&endDate=", end_date_str, UTC_hr,
  "&parameters=PM25,PM10&BBOX=-116.202774,44.045890,-103.722305,49.229925",
  "&dataType=C&format=text/csv&verbose=1&monitorType=0&includerawconcentrations=0",
  "&API_KEY=4A314159-4658-4690-8CE9-F716E5EABC20"
)
# Print URL to verify
cat(url)

col_names <- c("latitude", "longitude", "date_gmt", "parameter", "sample_measurement", 
               "units_of_measure", "site_name", "monitoring_agency", "AQSID", "Full_AQSID")

# Use tryCatch around GET call
tryCatch({
  response <- GET(url, timeout(30))  # Add timeout to prevent hanging
  
  if (status_code(response) == 200) {
    csv_data <- content(response, "text")
    
    air_quality_data <- read_csv(csv_data, col_names = FALSE)
    
    col_names <- c("latitude", "longitude", "date_gmt", "parameter", "sample_measurement", 
                   "units_of_measure", "site_name", "monitoring_agency", "AQSID", "Full_AQSID")
    colnames(air_quality_data) <- col_names
    
    AirNow <- air_quality_data %>%
      mutate(
        AQSID = as.character(AQSID),
        Full_AQSID = as.character(Full_AQSID),
        country_code = substr(Full_AQSID, 1, 3),
        state_code   = substr(Full_AQSID, 4, 5),
        county_code  = substr(Full_AQSID, 6, 8),
        site_number  = substr(Full_AQSID, 9, 12)
      ) %>%
      filter(country_code == "840", state_code == "30")
    
    print("AirNow data retrieved successfully.")
    
    #------------------------------Remove rows where sample_measurement is less than -900-------------------------
    AirNow <- AirNow %>%
      filter(sample_measurement >= -900 & parameter == "PM2.5") 
    
    saveRDS(AirNow, paste0("data//AirNow//", update_date, "_AirNow.rds"))
    
    
    #---------------------------Calculate running average------------------------------------
    AirNow_avg <- AirNow %>%
      mutate(date_mdt = date_gmt - hours(6)) %>%
      arrange(AQSID, date_mdt) %>%
      group_by(AQSID) %>%
      mutate(
        sample_measurement_24hr_avg = slide_period_dbl(
          .x = sample_measurement,
          .i = date_gmt,                  # Timestamp column
          .period = "hour",               # Sliding by the hour
          .every = 1,                     # Every hour
          .before = 23,                   # Look back 23 previous hours + current = 24 hours
          .complete = TRUE,
          .f = ~mean(.x, na.rm = TRUE)
        )
      ) %>%
      ungroup()
    
    site_coords <- AirNow_avg %>%
      dplyr::select(site_name, latitude, longitude) %>%
      dplyr::distinct()
    site_vect <- vect(site_coords, geom = c("longitude", "latitude"), crs = "EPSG:4326")
    counties_with_sites <- terra::extract(mt_v, site_vect)
    sites_and_counties <- cbind(site_coords, counties_with_sites[,-1]) %>%
      rename(county = NAME) %>%
      select(site_name, county)
    
    AirNow_avg <- AirNow_avg %>%
      left_join(sites_and_counties)
    
    
    saveRDS(AirNow_avg, paste0("data//AirNow//", update_date, "_AirNow_running_avg.rds"))
    
  } else {
    message(paste("Failed to fetch data. Status code:", status_code(response)))
  }
}, error = function(e) {
  message("GET request failed with error: ", e$message)
})





