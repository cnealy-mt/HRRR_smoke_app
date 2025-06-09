#Produce VENT RATE

# Try loading wind and mix height rasters safely
wind_file <- tryCatch(
  rast(paste0("data//WIND_1hr_max_fcst//WIND_1hr_max_fcst_", update_date, ".tif")),
  error = function(e) {
    cat(glue("Warning: Missing WIND file for {update_date}. Proceeding without it.\n"))
    return(NULL)
  }
)

mix_height_file <- tryCatch(
  rast(paste0("data//HPBL//HPBL_", update_date, ".tif")),
  error = function(e) {
    cat(glue("Warning: Missing HPBL file for {update_date}. Proceeding without it.\n"))
    return(NULL)
  }
)

# Proceed only if both files exist
if (!is.null(wind_file) & !is.null(mix_height_file)) {
  VENT_RATE <- wind_file * mix_height_file
  
  # Define output folder path
  folder_path <- "data/VENT_RATE"
  
  # Create folder if it doesn't exist
  if (!fs::dir_exists(folder_path)) {
    fs::dir_create(folder_path)
  }
  
  # Save result
  writeRaster(VENT_RATE, file.path(folder_path, paste0("VENT_RATE_", update_date, ".tif")), overwrite = TRUE)
  
  cat(glue("Successfully processed VENT RATE for {update_date}\n"))
} else {
  cat(glue("VENT RATE processing skipped for {update_date} due to missing input files.\n"))
}

