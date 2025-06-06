

# Define expected output paths
hourly_path <- paste0("data//county_hrly_avg//", update_date, "_county_hrly_avg.rds")
daily_path <- paste0("data//county_24hr_avg//", update_date, "_county_24hr_avg.rds")

# Check if either file exists
if (file_exists(hourly_path) && file_exists(daily_path)) {
  cat(glue("⏩ Output already exists for {update_date}, skipping calculation.\n"))
} else {
  # Try running the main script logic
  tryCatch({
    cat(glue("▶ Running calculations for {update_date}\n"))
    
    # Initialize the output data frame
    county_hourly_avg <- NULL
    
    var_loop <- c(as.character(vars$var_name), "VENT_RATE")
    
    # Loop over each variable
    for (this_var_name in var_loop) {
      
      folder_path <- paste0("data//", this_var_name) 
      filename <- fs::path(folder_path, paste0(this_var_name, "_", update_date, ".tif"))
      
      if (fs::file_exists(filename)) {
        message(glue::glue("File exists: loading {this_var_name} for {update_date}"))
        stack <- rast(filename)
        
        # Calculate County Hourly Avg
        temp_df <- lapply(seq_len(nlyr(stack)), function(j) {
          r <- stack[[j]]
          extracted <- terra::extract(r, mt_v, fun = mean, na.rm = TRUE)
          
          data.frame(
            county = mt_counties$NAME[extracted$ID],
            fcst_hour = j-1,
            timestamp_MDT = names(stack)[j],
            value = extracted[, 2]
          )
        }) %>% bind_rows()
        
        # Rename 'value' to this_var_name
        names(temp_df)[names(temp_df) == "value"] <- this_var_name
        
        # Merge into the main data frame
        if (is.null(county_hourly_avg)) {
          county_hourly_avg <- temp_df
        } else {
          county_hourly_avg <- dplyr::full_join(county_hourly_avg, temp_df,
                                                by = c("county", "fcst_hour", "timestamp_MDT"))
        }
        
      } else {
        message(glue::glue("File does not exist: adding empty column for {this_var_name}"))
        
        # If the main data frame exists, just add a column of NA
        if (!is.null(county_hourly_avg)) {
          county_hourly_avg[[this_var_name]] <- NA_real_
        }
      }
    }
    
    
    #--------------------------Calculate County 24-hr Avg-------------------------
    
    vent_rate_max_df <- county_hourly_avg %>%
      mutate(date = as.Date(timestamp_MDT)) %>%
      group_by(county, date) %>%
      summarise(
        VENT_RATE_max = max(VENT_RATE, na.rm = TRUE),
        .groups = "drop"
      )
    
    county_24hr_avg <- county_hourly_avg %>%
      mutate(date = as.Date(timestamp_MDT)) %>%
      group_by(county, date) %>%
      summarise(
        across(all_of(var_loop), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      left_join(vent_rate_max_df, by = c("county", "date"))
    
    first_date <- sort(unique(county_24hr_avg$date))[1]
    
    # Only the 2nd date (i.e. the day after the 12 UTC/6 MDT ) will have a complete day of data for 24-hr averages
    second_date <- sort(unique(county_24hr_avg$date))[2]
    
    county_18hr_avg_first_day <- county_24hr_avg %>%
      filter(date == first_date)
    county_24hr_avg_second_day <- county_24hr_avg %>%
      filter(date == second_date)
    
    #--------------------------Add AQI Data-------------------------
    
    # Define your breakpoints and category labels
    breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, Inf)
    labels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups",
                "Unhealthy", "Very Unhealthy", "Hazardous")
    
    county_hourly_avg <- county_hourly_avg %>%
      mutate(AQI_category = cut(MASSDEN,
                                breaks = breaks,
                                labels = labels,
                                right = TRUE, include.lowest = TRUE))
    
    # Add the AQI category column based on daily avg MASSDEN
    county_18hr_avg_first_day <- county_18hr_avg_first_day %>%
      mutate(AQI_category = cut(MASSDEN,
                                breaks = breaks,
                                labels = labels,
                                right = TRUE, include.lowest = TRUE))
    county_24hr_avg_second_day <- county_24hr_avg_second_day %>%
      mutate(AQI_category = cut(MASSDEN,
                                breaks = breaks,
                                labels = labels,
                                right = TRUE, include.lowest = TRUE))
    
    write_rds(county_hourly_avg, paste0("data//county_hrly_avg//", update_date, "_county_hrly_avg.rds"))
    write_rds(county_18hr_avg_first_day, paste0("data//county_24hr_avg//", update_date, "_updated_today_AQI_outlook.rds"))
    write_rds(county_24hr_avg_second_day, paste0("data//county_24hr_avg//", update_date, "_county_24hr_avg.rds"))
    
    cat(glue("✅ Finished writing outputs for {update_date}\n"))
  }, error = function(e) {
    cat(glue("❌ Error processing {update_date}: {e$message}\n"))
  })
}


