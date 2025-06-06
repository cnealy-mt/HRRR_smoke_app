# Variable utilities

#var_inp <- c("MASSDEN", "WIND_1hr_max_fcst", "TMP", "RH", "GUST", "HPBL", "PRATE", "VENT_RATE")

# Units lookup
get_unit <- function(var_inp) {
  units <- c(
    MASSDEN = "µg/m³",
    WIND_1hr_max_fcst = "m/s",
    TMP = "°F",
    RH = "%",
    GUST = "m/s",
    HPBL = "m",
    PRATE = "in/hr",
    VENT_RATE = "m²/s",
    VENT_RATE_max = "m²/s",
    VENT_RATE_max_today_update = "m²/s",
    VENT_WINDOW = "hours",
    VENT_WINDOW_today_update = "hours"
  )
  
  units[[var_inp]] %||% ""
}

# Variable Name lookup
get_var_name <- function(var_inp) {
  names <- c(
    MASSDEN = "Near-Surface Smoke",
    WIND_1hr_max_fcst = "Wind Speed",
    TMP = "Temperature",
    RH = "Relative Humidity",
    GUST = "Wind Gust",
    HPBL = "Boundary Layer Height",
    PRATE = "Precipitation Rate",
    VENT_RATE = "Ventilation Rate",
    VENT_RATE_max = "Ventilation Rate",
    VENT_RATE_max_today_update = "Ventilation Rate",
    VENT_WINDOW = "Ventilation Window",
    VENT_WINDOW_today_update = "Ventilation Window"
  )
  
  names[[var_inp]] %||% var_inp  # fallback to var_inp if no name found
}

# Combined label (e.g., "Temperature (°F)")
get_label <- function(var_inp) {
  paste0(get_var_name(var_inp), " (", get_unit(var_inp), ")")
}

# Safe operator
`%||%` <- function(a, b) if (!is.null(a)) a else b

#--------------------------Trend Utils-------------------
get_trend_label <- function(trend_duration, today) {
  today <- as.Date(today)
  names <- c(
    `1day` = paste0(today + 1, " (fcst date) minus ", today),
    `2day` = paste0(today + 1, " (fcst date) minus ", today - 1)
  )
  
  # Accessing the label based on the trend_duration argument
  names[trend_duration]
}

#get_trend_label("2day", today)

#-------------------------Hourly Fcst Labels------------
get_hourly_label <- function(today, fcst_hour) {
  # Convert today into a proper datetime, adding 6 hours
  today_dt <- as.POSIXct(today) + hours(6)
  
  # Add fcst_hour to the datetime
  result_dt <- today_dt + hours(fcst_hour)
  
  # Format the result nicely
  return(format(result_dt, "%Y-%m-%d %H:%M"))
}

# # Example usage:
# today <- format(Sys.Date(), "%Y-%m-%d")  # Get today's date as a string
# fcst_hour <- 3  # Example forecast hour
# 
# get_hourly_label(today, fcst_hour)


