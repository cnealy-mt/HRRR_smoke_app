# Calculate Max, Average, and Accumulation Outlook Rasters

max_vars <- c("MASSDEN", "TMP", "WIND_1hr_max_fcst")
avg_vars <- c("MASSDEN")
acc_vars <- c("PRATE")

all_vars <- list(
  max = max_vars,
  avg = avg_vars,
  acc = acc_vars
)

for (type in names(all_vars)) {
  for (var in all_vars[[type]]) {
    
    stack_name <- paste0(var, "_stack")
    if (!exists(stack_name, envir = .GlobalEnv)) {
      cat(glue("Warning: Missing raster for {var}. Skipping processing.\n"))
      next
    }
    
    rast <- get(stack_name, envir = .GlobalEnv)
    n_layers <- nlyr(rast)
    layer_index <- 1:n_layers
    
    # Each layer timestamp
    layer_datetime <- local_runtime + hours(layer_index)
    
    # Build reference table
    layer_ref <- tibble(
      layer = layer_index,
      datetime = layer_datetime,
      date = as.Date(layer_datetime, tz = "America/Denver"),
      lead_time = as.integer(difftime(date, as.Date(local_runtime), units = "days"))
    )
    
    # Find unique lead times
    unique_leads <- sort(unique(layer_ref$lead_time))
    
    # Ensure folder exists and remove old files once per var
    var_path <- glue("data/{var}")
    ensure_dir(var_path)
    old_files <- fs::dir_ls(var_path, regexp = "\\.tif$")
    old_files <- old_files[!grepl(glue("^{update_date}"), basename(old_files))]
    if (length(old_files) > 0) fs::file_delete(old_files)
    
    for (lt in unique_leads) {
      layers_to_keep <- layer_ref$layer[layer_ref$lead_time == lt]
      if (length(layers_to_keep) < 16) {
        cat(glue("Skipping lead_time {lt}: only {length(layers_to_keep)} hours available\n"))
        next
      }
      
      r_sub <- rast[[layers_to_keep]]
      out_rast <- switch(type,
                         max = max(r_sub),
                         avg = mean(r_sub),
                         acc = sum(r_sub))
      
      out_name <- glue("{var_path}/{update_date}_{var}_{type}_lead{lt}.tif")
      writeRaster(out_rast, out_name, overwrite = TRUE)
      cat(glue("Saved {out_name}\n"))
    }
  }
}


