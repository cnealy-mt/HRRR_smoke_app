# Bias Plot Module UI
bias_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("bias_plot"), height = 400)
}



# Module Server
bias_plot_ModuleServer <- function(id, year, month, variable, lead_time) {
  moduleServer(id, function(input, output, session) {
    output$bias_plot <- renderHighchart({
      
      daily_model_performance <- readRDS("data/model_performance/daily_model_performance.rds") %>%
        filter(lubridate::month(date) %in% 5:10 &
                 lubridate::year(date) == year())
      
      # Use standard `if` to choose the columns based on lead_time (a scalar input)
      if (lead_time() == "zero_day") {
        daily_model_performance <- daily_model_performance %>%
          mutate(
            model_value = avg_HRRR_ug_m3_today_update,
            accuracy_value = accuracy_update
          )
      } else {
        daily_model_performance <- daily_model_performance %>%
          mutate(
            model_value = avg_HRRR_ug_m3,
            accuracy_value = accuracy
          )
      }
      
      bias <- daily_model_performance %>%
        select(model_value, avg_sample_measurement, obs_AQI, accuracy_value) %>%
        drop_na() %>%
        mutate(bias = model_value - avg_sample_measurement) %>%
        group_by(obs_AQI) %>%
        summarise(
          mean_bias = mean(bias),
          sample_count = n(),
          accuracy_pct = mean(accuracy_value %in% c(-1, 0, 1)) * 100  # Percent of predictions within ±1 category          .groups = "drop"
        )
      
      
      # 1. Map AQI numeric levels to labels and colors
      aqi_labels <- c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous")
      aqi_colors <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
      
      # 2. Prepare chart data: filter to only the AQI levels you have data for
      chart_data <- bias %>%
        mutate(
          category = aqi_labels[obs_AQI],
          color = aqi_colors[obs_AQI]
        )
      
      # 3. Build the highchart
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "HRRR Smoke Bias by AQI Category (µg/m³)") %>%
        hc_legend(enabled = FALSE) %>%  # <- Hide the legend and button
        hc_xAxis(
          categories = chart_data$category,
          title = list(text = "Observed AQI Category")
        ) %>%
        hc_yAxis(
          title = list(text = "Mean Bias (HRRR - Obs)"),
          plotLines = list(
            list(value = 0, color = "#000000", width = 2)  # Horizontal line at 0
          )
        ) %>%
        hc_add_series(
          name = "Mean Bias",
          data = pmap(
            list(
              y = chart_data$mean_bias,
              color = chart_data$color,
              sample_count = chart_data$sample_count,
              accuracy_pct = chart_data$accuracy_pct
            ),
            ~ list(
              y = ..1,
              color = ..2,
              sample_count = ..3,
              accuracy_pct = ..4
            )
          )
        ) %>%
        hc_tooltip(
          pointFormat = paste0(
            "Mean Bias: <b>{point.y:.2f}</b><br>",
            "Sample Count: <b>{point.sample_count}</b><br>",
            "Accuracy (Within 1 AQI Cat.): <b>{point.accuracy_pct:.1f}%</b>"
          )
        ) %>%
        hc_plotOptions(
          column = list(
            dataLabels = list(enabled = TRUE, format = "{point.y:.1f}"),
            pointPadding = 0,   # No space between the columns
            groupPadding = 0    # No space between the groups
          )
        )

    })
  })
}



