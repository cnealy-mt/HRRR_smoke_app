model_performance_timeseries_ModuleUI <- function(id) {
  ns <- NS(id)
  highchartOutput(ns("model_performance_timeseries"), height = 400)
}

model_performance_timeseries_ModuleServer <- function(id, site_name_mp, start_date_mp, end_date_mp, lead_time) {
  moduleServer(id, function(input, output, session) {
    output$model_performance_timeseries <- renderHighchart({
      

      
      # filter to only include data up to latest AirNow data and between calendar selection input
      model_performance_timeseries <- hourly_model_performance %>% # loaded in plot_utils.R
        filter(site_name == site_name_mp() &
                 date >= start_date_mp() & date <= end_date_mp())
      
      # Use standard `if` to conditionally assign column mappings
      if (lead_time() == 0) {
        df <- model_performance_timeseries %>%
          mutate(
            local_time_utc = lubridate::force_tz(local_time, tzone = "UTC"), # this way highcharter will put in correct timezone since it thinks local_time is UTC
            model_point_hourly = model_smoke_lead0,
            model_point_24hr_running = `24hr_avg_model_smoke_lead0`,
            AirNow_hourly = airnow_obs,           # These stay the same
            AirNow_24hr_running = `24hr_avg_airnow_obs`
          )
      } else {
        df <- model_performance_timeseries %>%
          mutate(
            local_time_utc = lubridate::force_tz(local_time, tzone = "UTC"), # this way highcharter will put in correct timezone since it thinks local_time is UTC
            model_point_hourly = model_smoke_lead1,
            model_point_24hr_running = `24hr_avg_model_smoke_lead1`,
            AirNow_hourly = airnow_obs,           # These stay the same
            AirNow_24hr_running = `24hr_avg_airnow_obs`
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
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = model_point_hourly
            ) %>%
            list_parse2(),
          color = "#F54D28"
        ) %>%
        hc_add_series(
          name = "AirNow Hourly",
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = AirNow_hourly
            ) %>%
            list_parse2(),
          color = "#004A98"
        ) %>%
        hc_add_series(
          name = "HRRR 24-hr Running Avg",
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
              y = model_point_24hr_running
            ) %>%
            list_parse2(),
          color = "#F54D28",
          lineWidth = 5,
          opacity = .5,
          marker = list(enabled = FALSE)
        ) %>%
        hc_add_series(
          name = "AirNow 24-hr Running Avg",
          data = df %>%
            transmute(
              x = datetime_to_timestamp(local_time_utc),
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



  
