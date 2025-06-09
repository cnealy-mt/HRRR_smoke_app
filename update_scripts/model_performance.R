# Ensure folders exist
ensure_dir("data/model_performance")

compare_date <- as.Date(compare_date)
yesterday_date <- compare_date + 1

#--------------------------Load AirNow data-------------------------
airnow_path <- paste0("data//AirNow//", update_date, "_AirNow.rds")

if (file.exists(airnow_path)) {
  cat(glue("✅ AirNow data found: {airnow_path}\n"))
  
  # Load AirNow data
  AirNow <- readRDS(airnow_path)
  
  # Get monitoring site coordinates
  site_coords <- AirNow %>%
    dplyr::select(site_name, latitude, longitude) %>%
    dplyr::distinct()
  
  site_vect <- vect(site_coords, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  
  #--------------------------Check for Smoke Files-------------------------
  
  file_compare <- paste0("data//MASSDEN//MASSDEN_", compare_date, ".tif")
  file_yesterday <- paste0("data//MASSDEN//MASSDEN_", yesterday_date, ".tif")
  
  if (file.exists(file_compare) && file.exists(file_yesterday)) {
    cat(glue("✅ Both smoke raster files found:\n - {file_compare}\n - {file_yesterday}\n"))
    
    # Load rasters
    smoke_compare <- rast(file_compare)
    smoke_yesterday <- rast(file_yesterday)
    
    # Extract values
    values_compare <- terra::extract(smoke_compare, site_vect)
    values_yesterday <- terra::extract(smoke_yesterday, site_vect)
    
    # Add site names back
    values_compare <- cbind(site_coords, values_compare)
    values_yesterday <- cbind(site_coords, values_yesterday)
    
    #--------------------------------Restructure--------------------------------
    run_time <- as.POSIXct(paste0(compare_date, " 06:00:00"))
    
    reshape_smoke_data <- function(values) {
      # Convert to long format and ensure correct timezone handling
      smoke_long <- values %>%
        pivot_longer(
          cols = -c(site_name, latitude, longitude, ID),  
          names_to = "timestamp_MDT",
          values_to = "smoke_ug_m3"
        ) %>%
        mutate(
          date_mdt = as.POSIXct(timestamp_MDT),
          date_gmt = with_tz(timestamp_MDT, tzone = "UTC"),
          fcst_date = compare_date,
          fcst_hour = as.numeric(difftime(timestamp_MDT, run_time, units = "hours"))  # << add this line
        ) %>%
        select(-ID)
      
      # Extract date in MDT and ensure it's in MDT timezone
      smoke_long <- smoke_long %>%
        mutate(
          date_only = as.Date(format(date_mdt))  # Extract date in MDT timezone
        )
      
      # Filter data for the second date
      filtered_data <- smoke_long %>%
        filter(date_only == yesterday_date) %>%
        select(site_name, latitude, longitude, date_mdt, smoke_ug_m3)
    }
    
    filtered_data_compare <- reshape_smoke_data(values_compare)
    filtered_data_yesterday <- reshape_smoke_data(values_yesterday)
    
    missing_rows <- anti_join(
      filtered_data_compare,
      filtered_data_yesterday,
      by = c("site_name", "date_mdt")
    )
    
    filtered_data_yesterday <- bind_rows(filtered_data_yesterday, missing_rows) %>%
      arrange(site_name, date_mdt) %>%
      rename(smoke_ug_m3_today_update = smoke_ug_m3)
    
    filtered_data <- left_join(filtered_data_compare, filtered_data_yesterday)
    
    #--------------------------Merge Site Point Fcst with Obs-------------------------
    
    AirNow_merge <- AirNow %>%
      select(site_name, date_gmt, sample_measurement) %>%
      mutate(
        date_mdt = as.POSIXct(date_gmt, tz = "Etc/GMT+6")
      ) %>%
      select(-date_gmt)
    
    head(AirNow_merge)
    
    monitor_point_data <- inner_join(
      filtered_data,
      AirNow_merge,
      by = c("site_name", "date_mdt")
    )
    
    
    # library(ggplot2)
    # 
    # ggplot(monitor_point_data, aes(x = smoke_ug_m3, y = sample_measurement)) +
    #   geom_point(color = "blue", size = 2, alpha = 0.7) +
    #   labs(
    #     title = "Sample Measurement vs Smoke Forecast",
    #     x = "Forecasted Smoke (ug/m3)",
    #     y = "Observed Sample Measurement (ug/m3)"
    #   ) +
    #   theme_minimal()
    
    #--------------------------Load Archive Data-------------------------
    
    hourly <- readRDS("data//archive//county_hourly_archive.rds")
    daily <- readRDS("data//archive//county_24hr_archive.rds")
    
    #--------------------------Filter for Counties with Sites-------------------------
    counties_with_sites <- terra::extract(mt_v, site_vect)
    sites_and_counties <- cbind(site_coords, counties_with_sites[,-1]) %>%
      rename(county = NAME) %>%
      select(site_name, county)
    
    #Filter to only include most recent date (will append later to running model performance data)
    hourly <- hourly %>%
      filter(county %in% c(sites_and_counties$county)) %>%
      mutate(date_mdt = as.POSIXct(date_mdt, format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+6")) %>%
      filter(date == as.Date(update_date)-1)
    
    daily <- daily %>%
      filter(county %in% c(sites_and_counties$county)) %>%
      filter(date == as.Date(update_date)-1)
    
    
    #--------------------------Combine Site Point Data and County Averages-------------------------
    
    #hourly
    monitor_point_data_hourly <- monitor_point_data %>%
      left_join(sites_and_counties, by = "site_name")
    
    hourly_model_performance <- monitor_point_data_hourly %>%
      left_join(hourly, by = c("county", "date_mdt"))
    
    #daily 
    monitor_point_data_daily <- monitor_point_data %>%
      group_by(site_name) %>%
      summarise(
        avg_HRRR_ug_m3 = mean(smoke_ug_m3, na.rm = TRUE),
        avg_HRRR_ug_m3_today_update = mean(smoke_ug_m3_today_update, na.rm = TRUE),
        avg_sample_measurement = mean(sample_measurement, na.rm = TRUE)
      ) %>%
      left_join(sites_and_counties, by = "site_name")
    
    daily_model_performance <- monitor_point_data_daily %>%
      left_join(daily, by = c("county"))
    
    #--------------------------Add AQI Category Accuracy (daily only)-------------------------
    
    breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)
    
    daily_model_performance <- daily_model_performance %>%
      mutate(
        AQI_HRRR = cut(avg_HRRR_ug_m3, breaks = breaks, labels = FALSE, right = FALSE),
        AQI_HRRR_today_update = cut(avg_HRRR_ug_m3_today_update, breaks = breaks, labels = FALSE, right = FALSE),
        obs_AQI = cut(avg_sample_measurement, breaks = breaks, labels = FALSE, right = FALSE),
        accuracy = AQI_HRRR - obs_AQI,
        accuracy_update = AQI_HRRR_today_update - obs_AQI
      ) 
    
    #--------------------------Save Model Performance-------------------------
    # Define date window
    start_date <- as.Date(update_date) - lubridate::years(5)
    end_date <- as.Date(update_date)
    
    # Function to append and save RDS
    append_and_save <- function(new_data, path, group_vars) {
      if (file.exists(path)) {
        existing_data <- readRDS(path)
        combined_data <- bind_rows(existing_data, new_data) %>%
          mutate(na_count = rowSums(is.na(.))) %>%
          group_by(across(all_of(group_vars))) %>%
          slice_min(order_by = na_count, with_ties = FALSE) %>%
          ungroup() %>%
          select(-na_count)
      } else {
        combined_data <- new_data
      }
      
      saveRDS(combined_data, path)
    }
    
    # Save hourly data (group by site_name + date_mdt)
    append_and_save(hourly_model_performance, 
                    "data/model_performance/hourly_model_performance.rds",
                    group_vars = c("site_name", "date_mdt"))
    test <- readRDS("data/model_performance/hourly_model_performance.rds")
    
    # Save daily data (group by site_name + date)
    append_and_save(daily_model_performance, 
                    "data/model_performance/daily_model_performance.rds",
                    group_vars = c("site_name", "date"))
    test <- readRDS("data/model_performance/daily_model_performance.rds")
    
    #-----------------------Calculate Running 24hr Avg-------------------
    hourly_model_performance <- readRDS("data/model_performance/hourly_model_performance.rds")
    
    # Recalculate and overwrite 24-hour averages for the entire dataset
    model_performance_24hr_update <- hourly_model_performance %>%
      arrange(site_name, date_mdt) %>%
      group_by(site_name) %>%
      mutate(
        HRRR_24hr_county = slide_dbl(MASSDEN, mean, .before = 23, .complete = FALSE),
        HRRR_24hr_point = slide_dbl(smoke_ug_m3, mean, .before = 23, .complete = FALSE),
        HRRR_24hr_today_update_point = slide_dbl(smoke_ug_m3_today_update, mean, .before = 23, .complete = FALSE),
        AirNow_24hr = slide_dbl(sample_measurement, mean, .before = 23, .complete = FALSE)
      ) %>%
      ungroup()

    saveRDS(model_performance_24hr_update, "data/model_performance/hourly_model_performance.rds")
    
  } else {
    # Handle missing raster files
    if (!file.exists(file_compare)) {
      cat(glue("⚠️ Missing smoke raster file for compare_date: {file_compare}\n"))
    }
    if (!file.exists(file_yesterday)) {
      cat(glue("⚠️ Missing smoke raster file for yesterday_date: {file_yesterday}\n"))
    }
    cat("⛔ Aborting: Both smoke raster files must be present to proceed.\n")
  }
  
} else {
  # Handle missing AirNow data
  cat(glue("⛔ Skipping model performance script. AirNow file not found: {airnow_path}\n"))
}







