#------------------------Calculate Vent Rate WINDOW---------------------
tomorrow_run <- as.Date(update_date)
tomorrow_filter <- format(as.Date(tomorrow_run) + 1, "%Y-%m-%d")
today_filter <- format(as.Date(update_date), "%Y-%m-%d")
folder_path <- paste0("data//VENT_RATE")
print(folder_path)

# Try to load raster safely
tomorrow_rast <- tryCatch(
  rast(paste0(folder_path, "//VENT_RATE_", tomorrow_run, ".tif")),
  error = function(e) {
    cat(glue("Warning: Missing VENT RATE raster for {tomorrow_run}. Skipping processing.\n"))
    return(NULL)
  }
)

process_vent_window <- function(rast, filter_str, update_date, suffix = "", mt_v) {
  layer_names <- names(rast)
  layers_to_keep <- grepl(filter_str, layer_names)
  filtered_rast <- rast[[layers_to_keep]]
  
  valid_count_rast <- app(filtered_rast, fun = function(x) sum(!is.na(x)))
  marginal_rast <- app(filtered_rast, fun = function(x) sum(x > 2350, na.rm = TRUE))
  good_rast     <- app(filtered_rast, fun = function(x) sum(x > 4700, na.rm = TRUE))
  
  marginal_rast[valid_count_rast < 1] <- NA
  good_rast[valid_count_rast < 1]     <- NA
  
  # Ensure folders exist
  ensure_dir("data/VENT_WINDOW")
  
  writeRaster(marginal_rast, paste0("data/VENT_WINDOW/", update_date, "_marginal_rast", suffix, ".tif"), overwrite = TRUE)
  writeRaster(good_rast, paste0("data/VENT_WINDOW/", update_date, "_good_rast", suffix, ".tif"), overwrite = TRUE)
  
  cat(glue("Successfully processed VENT RATE WINDOW{if (suffix != '') ' ' else ''}{suffix} for {update_date}\n"))
  
  marginal_means <- terra::extract(marginal_rast, mt_v, fun = mean, na.rm = TRUE)
  good_means     <- terra::extract(good_rast, mt_v, fun = mean, na.rm = TRUE)
  
  VENT_WINDOW_marginal <- data.frame(
    county = mt_v$NAME[marginal_means$ID],
    VENT_WINDOW = marginal_means[[2]]
  )
  VENT_WINDOW_good <- data.frame(
    county = mt_v$NAME[good_means$ID],
    VENT_WINDOW = good_means[[2]]
  )
  
  saveRDS(VENT_WINDOW_marginal, paste0("data/VENT_WINDOW/", update_date, "_VENT_WINDOW_marginal_county_avg", suffix, ".rds"))
  saveRDS(VENT_WINDOW_good, paste0("data/VENT_WINDOW/", update_date, "_VENT_WINDOW_good_county_avg", suffix, ".rds"))
}

if (!is.null(tomorrow_rast)) {
  process_vent_window(tomorrow_rast, tomorrow_filter, update_date, "", mt_v)
  process_vent_window(tomorrow_rast, today_filter, update_date, "_today_update", mt_v)
} else {
  cat(glue("VENT WINDOW processing skipped for {tomorrow_run} due to missing raster file.\n"))
}





