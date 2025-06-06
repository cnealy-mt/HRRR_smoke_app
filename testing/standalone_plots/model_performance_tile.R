

# Input: year & month
year <- 2024
month <- 9  # July
variable <- "avg_HRRR_ug_m3" #accuracy, avg_HRRR_ug_m3, avg_sample_measurement

# Step 1: Get all days in the selected month
date_range <- seq(
  as.Date(sprintf("%d-%02d-01", year, month)),
  as.Date(sprintf("%d-%02d-01", year, month)) %m+% months(1) - days(1),
  by = "day"
)

# Step 2: Filter the data for the selected month
daily_model_performance <- readRDS("data/model_performance/daily_model_performance.rds")
data <- daily_model_performance %>%
  select(site_name, date, all_of(variable)) %>%
  filter(as.Date(date) %in% date_range)

# Step 3: Ensure all site/day combinations exist
site_levels <- unique(data$site_name)
complete_data <- expand.grid(
  site_name = site_levels,
  date = date_range
)

# Step 4: Join to get full dataset
data_full <- complete_data %>%
  left_join(data, by = c("site_name", "date")) %>%
  mutate(
    day = day(date),
    x = day,  # X-axis is day of month (1–31)
    y = match(site_name, site_levels),  # numeric y for heatmap
    value = get(variable)  # Pull value from variable column
  )

# Step 5: Prepare data for Highcharts
heatmap_data <- data_full %>%
  transmute(
    x = x - 1,  # JS 0-based indexing
    y = y - 1,
    value = value
  ) %>%
  list_parse()

# Step 6: get color scale

if (variable == "accuracy") {
  aqi_accuracy_values <- -5:5
  aqi_accuracy_colors <- c(
    colorRampPalette(c("#922b21", "#fcf3cf"))(5),  # 5 greens (-5 to -1)
    "white",                                      # 0
    colorRampPalette(c("#d1f2eb", "#1f618d"))(5)   # 5 reds (+1 to +5)
  )

    data_classes_list <- lapply(seq_along(aqi_accuracy_values), function(i) {
    val <- aqi_accuracy_values[i]
    list(from = val, to = val, color = aqi_accuracy_colors[i], name = paste(val))
  })
  
} else {
  aqi_breaks <- c(0, 9, 35.4, 55.4, 125.4, 225.4, 1000)
  aqi_colors <- c(
    "#00E400",  # Good
    "#FFFF00",  # Moderate
    "#FF7E00",  # USG
    "#FF0000",  # Unhealthy
    "#8F3F97",  # Very Unhealthy
    "#7E0023"   # Hazardous
  )
  
  # Create classes between breaks
  data_classes_list <- lapply(seq_len(length(aqi_breaks) - 1), function(i) {
    list(
      from = aqi_breaks[i],
      to = aqi_breaks[i + 1],
      color = aqi_colors[i],
      name = paste0(aqi_breaks[i], "–", aqi_breaks[i + 1])
    )
  })
}

# Step 7: Plot the heatmap
plot_title <- switch(
  variable,
  "accuracy" = "AQI Difference - HRRR minus Monitor Obs",
  "avg_HRRR_ug_m3" = "HRRR Concentration",
  "avg_sample_measurement" = "Monitor Obs Concentration",
  "Unknown Variable"
)

highchart() %>%
  hc_chart(type = "heatmap") %>%
  hc_title(text = plot_title) %>%
  hc_xAxis(
    title = list(text = "Day of Month"),
    categories = as.character(unique(data_full$x)),  # Back to 1-based for labels
    tickInterval = 1
  ) %>%
  hc_yAxis(
    title = list(text = "Site"),
    categories = site_levels,
    reversed = TRUE
  ) %>%
  hc_add_series(
    data = heatmap_data,
    type = "heatmap"
  ) %>%
  hc_colorAxis(
    dataClasses = data_classes_list,
    nullColor = "#e0e0e0"
  ) %>%
  hc_tooltip(formatter = JS(sprintf(
    "function() {
     var year = %d;
     var month = %d - 1;  // JS months are 0-indexed
     var day = this.point.x + 1;
     var date = new Date(year, month, day);
     var dateString = date.toISOString().split('T')[0];  // 'YYYY-MM-DD'
     return 'Site: <b>' + this.series.yAxis.categories[this.point.y] + '</b>' +
            '<br>Date: <b>' + dateString + '</b>' +
            '<br>Value: <b>' + this.point.value.toFixed(1) + '</b>';
   }", year, month
  )))
