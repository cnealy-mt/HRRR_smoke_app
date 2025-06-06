
# Module UI
worm_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_grid"))
  )
}

# Server
worm_plot_ModuleServer <- function(id, airnow_today, AirNow_select) {
  moduleServer(id, function(input, output, session) {
   
    # switched from using 'today' (i.e., update_date, as defined in top level of app) to system date/time for operational use; 
    # ensures that hourly monitoring data is always up to date, but the forecasts are correctly labeled (i.e., displays yesterdays forecasts and labels if new model run hasn't been updated for the day) 
    data <- reactive({
      # Choose file name based on AirNow_select()
      file_suffix <- if (AirNow_select() == "running_avg") {
        "_AirNow_running_avg.rds"
      } else {
        "_AirNow.rds"
      }
      
      file_path <- paste0("data/AirNow/", airnow_today, file_suffix)
      
      df <- readRDS(file_path) %>%
        mutate(
          date_mdt = date_gmt - hours(6),
          date_mdt_ms = as.numeric(date_mdt) * 1000
        ) %>%
        filter(date_mdt >= as.Date(airnow_today) - hours(48))
      
      if (AirNow_select() == "hourly") {
        df <- df %>% mutate(sample_value = trunc(sample_measurement * 10) / 10)
      } else {
        df <- df %>% mutate(sample_value = trunc(sample_measurement_24hr_avg * 10) / 10)
      }
      
      df
    })
    
    # Zones
    zones <- get_zones("MASSDEN")
    
    # Reactive chart list
    charts <- reactive({
      df <- data()
      site_list <- split(df, df$site_name)
      
      imap(site_list, function(df_site, site) {
        highchart() %>%
          hc_add_series(
            data = df_site,
            type = "area",
            hcaes(x = date_mdt_ms, y = sample_value),
            name = if (AirNow_select() == "hourly") "Hourly PM2.5" else "24-hr Avg PM2.5",
            zones = zones,
            lineWidth = 5,
            fillOpacity = 0.5
          ) %>%
          hc_chart(margin = c(30, 0, 30, 50)) %>%
          hc_title(
            text = site,
            style = list(fontSize = "10px", fontWeight = "bold")
          ) %>%
          hc_xAxis(
            title = list(text = NULL),
            type = "datetime",
            labels = list(format = "{value:%b %d %H:%M}")
          ) %>%
          hc_yAxis(
            title = list(text = "µg/m³"),
            labels = list(enabled = TRUE)
          ) %>%
          hc_tooltip(
            pointFormat = "<b>{point.y}</b> µg/m³"
          ) %>%
          hc_legend(enabled = FALSE)
      })
    })
    
    # UI layout
    output$chart_grid <- renderUI({
      req(charts())
      
      bslib::layout_columns(
        col_widths = 3,
        gap = "1rem",
        !!!lapply(seq_along(charts()), function(i) {
          highchartOutput(session$ns(paste0("chart_", i)), height = "200px")
        })
      )
    })
    
    # Output each chart
    observe({
      req(charts())
      lapply(seq_along(charts()), function(i) {
        output[[paste0("chart_", i)]] <- renderHighchart({
          charts()[[i]]
        })
      })
    })
  })
}
