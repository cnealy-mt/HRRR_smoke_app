model_performance_timeseries_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("model_performance_timeseries"), height = 400)
}

model_performance_timeseries_ModuleServer <- function(id, site_name_mp, start_date_mp, end_date_mp, lead_time) {
  moduleServer(id, function(input, output, session) {
    output$model_performance_timeseries <- renderHighchart({
      
      model_performance_timeseries <- readRDS("data/model_performance/hourly_model_performance.rds") %>%
        filter(site_name == site_name_mp() &
                 date >= start_date_mp() & date <= end_date_mp()) %>%
        mutate(
          date_utc = date_mdt - hours(6)
        )
      
      # Use standard `if` to conditionally assign column mappings
      if (lead_time() == "zero_day") {
        model_performance_timeseries <- model_performance_timeseries %>%
          mutate(
            HRRR_point_hourly = smoke_ug_m3_today_update,
            HRRR_point_24hr_running = HRRR_24hr_today_update_point,
            AirNow_hourly = sample_measurement,           # These stay the same
            AirNow_24hr_running = AirNow_24hr
          )
      } else {
        model_performance_timeseries <- model_performance_timeseries %>%
          mutate(
            HRRR_point_hourly = smoke_ug_m3,
            HRRR_point_24hr_running = HRRR_24hr_point,
            AirNow_hourly = sample_measurement,
            AirNow_24hr_running = AirNow_24hr
          )
      }

      
      # Assume your tibble is called model_performance_timeseries
      
      # Create the highchart
      highchart() %>%
        hc_chart(type = "line") %>%
        hc_title(text = "Model vs. Observed Concentrations Over Time") %>%
        hc_xAxis(
          type = "datetime",
          title = list(text = "Time")
        ) %>%
        hc_yAxis(
          title = list(text = "PM2.5 Concentration (µg/m³)")
        ) %>%
        hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          formatter = JS(
            "function () {
       let s = '<b>' + Highcharts.dateFormat('%Y-%m-%d %H:%M', this.x) + '</b>';
       this.points.forEach(function(point) {
         s += '<br/>' + point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 1) + '</b>';
       });
       return s;
     }"
          )
        ) %>%
        hc_add_series(
          name = "HRRR Hourly",
          data = model_performance_timeseries %>%
            transmute(
              x = datetime_to_timestamp(date_utc),
              y = HRRR_point_hourly
            ) %>%
            list_parse2(),
          color = "#F54D28"
        ) %>%
        hc_add_series(
          name = "AirNow Hourly",
          data = model_performance_timeseries %>%
            transmute(
              x = datetime_to_timestamp(date_utc),
              y = AirNow_hourly
            ) %>%
            list_parse2(),
          color = "#004A98"
        ) %>%
        hc_add_series(
          name = "HRRR 24-hr Running Avg",
          data = model_performance_timeseries %>%
            transmute(
              x = datetime_to_timestamp(date_utc),
              y = HRRR_point_24hr_running
            ) %>%
            list_parse2(),
          color = "#F54D28",
          lineWidth = 5,
          opacity = .5,
          marker = list(enabled = FALSE)
        ) %>%
        hc_add_series(
          name = "AirNow 24-hr Running Avg",
          data = model_performance_timeseries %>%
            transmute(
              x = datetime_to_timestamp(date_utc),
              y = AirNow_24hr_running
            ) %>%
            list_parse2(),
          color = "#004A98",
          lineWidth = 5,
          opacity = .5,
          marker = list(enabled = FALSE)
        ) 
      
    })
  })
}



  
