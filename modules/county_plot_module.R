
# Module UI
county_plot_ModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
      uiOutput(ns("chart_grid"))
  )
}


county_plot_ModuleServer <- function(id, county, today) {
  moduleServer(id, function(input, output, session) {
    
    hourly_data <- readRDS(paste0("data/county_hrly_avg/",today,"_county_hrly_avg.rds")) 

    # Define the variables to plot (excluding non-numeric or non-variable columns)
    vars_to_plot <- setdiff(names(hourly_data), c("county", "fcst_hour", "time_local", "AQI_category"))
    
    # Reactive data filtered by selected county
    data_filtered <- reactive({
      req(county())
      hourly_data %>%
        filter(county == county()) 
    })
    
    # Generate time series charts for each variable
    charts <- reactive({
      req(data_filtered())
      
      map(vars_to_plot, function(var) {
        data_var <- data_filtered() 
        
        data_var <- data_var %>%
          mutate(local_time_naive = force_tz(time_local, "UTC"),  # reinterpret as UTC
                 local_time_ms = as.numeric(local_time_naive) * 1000)

        zones <- get_zones(var)
        
        highchart() %>%
          hc_add_series(
            data = data_var,
            type = "area",  # Change from "line" to "area"
            hcaes(x = local_time_ms, y = .data[[var]]),
            name = var,
            lineWidth = 2,
            zones = zones,
            fillOpacity = 0.4  # Optional: controls how transparent the fill is
          ) %>%
          hc_title(
            text = get_var_name(var),
            style = list(fontSize = "12px", fontWeight = "bold")
          ) %>%
          hc_chart(margin = c(30, 0, 30, 50)) %>%
          hc_xAxis(
            title = list(text = NULL),
            type = "datetime",
            labels = list(format = "{value:%b %d %H:%M}")
          ) %>%
          hc_yAxis(
            title = list(text = get_unit(var)),
            labels = list(format = "{value:.1f}")
          ) %>%
          hc_tooltip(
            valueDecimals = 1,
            valueSuffix = paste0(" ", get_unit(var))
          ) %>%
          hc_legend(enabled = FALSE)
        
        
        
      }) %>%
        setNames(vars_to_plot)
    })
    
    # Dynamically render chart grid UI
    output$chart_grid <- renderUI({
      req(charts())
      
      bslib::layout_columns(
        col_widths = 4,  # Bootstrap 12 columns / 3 = 4
        gap = "1rem",
        !!!lapply(seq_along(charts()), function(i) {
          highchartOutput(session$ns(paste0("chart_", i)), height = "250px")
        })
      )
    })
    
    # Render charts
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

