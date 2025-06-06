hourly_map_ModuleUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("hourly_map"), height = 600)
}


hourly_map_ModuleServer <- function(id, today, var_inp, fcst_hour, transparency, active_tab) {
  moduleServer(id, function(input, output, session) {
    today <- as.Date(today)
    
    # 1️⃣ Render empty base map once
    observeEvent(active_tab(), { #added observeEvent and invalidateLater delay to make sure Leaflet doesn't break when switching back and forth between map tabs...Leaflet container struggles with being hidden and re-rendering
      if (active_tab() == "Hourly Forecast") {
        invalidateLater(300, session)
        output$hourly_map <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            setView(lng = -110.0, lat = 47.0, zoom = 7)
        })
      }
    })
    
    ## Debugging
    # observeEvent(list(var_inp(), transparency(), fcst_hour()), {
    #   print(var_inp())
    #   print(transparency())
    #   print(fcst_hour())
    # })
    
    # 2️⃣ Observe input changes and update map layers
    observe({
      req(active_tab() == "Hourly Forecast")
      req(var_inp(), transparency(), fcst_hour())  # Include fcst_hour() as required input
      
      forecast_label <- get_hourly_label(today, fcst_hour())
      print(forecast_label)
      
      map_proxy <- leafletProxy("hourly_map", session)
      
      # Clear old layers
      map_proxy %>% clearShapes() %>% clearImages() %>% clearControls()
      
      # AQI 24hr
      if (var_inp() == "AQI_hourly") {
        file_path <- paste0("data/county_hrly_avg/", today, "_county_hrly_avg.rds")
      
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Hourly data not available for", today, "model run"))
          return()
        }
        
        county_hourly_avg <- readRDS(file_path)
        
        # Filter for the selected forecast hour
        county_hourly_avg_filtered <- county_hourly_avg %>%
          filter(fcst_hour == fcst_hour())  # Ensure only selected hour is mapped
        
        mt_counties_AQI <- mt_counties %>%
          left_join(county_hourly_avg_filtered, by = c("NAME" = "county"))
        
        smoke_pal <- colorBin(
          palette = aqi_colors,
          domain = mt_counties_AQI$MASSDEN,
          bins = aqi_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        map_proxy %>%
          addPolygons(
            data = mt_counties_AQI,
            fillColor = ~smoke_pal(MASSDEN),
            fillOpacity = as.numeric(transparency()),
            color = "black",
            weight = .5,
            popup = ~paste0(
              "<strong>", NAME, " County</strong><br>",
              "Smoke Concentration: ", round(MASSDEN, 1), " µg/m³<br>",
              "AQI (1-hr): ", AQI_category
            )
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                               padding: 4px 8px; border-radius: 4px;
                               font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   forecast_label, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = smoke_pal,
            values = mt_counties_AQI$MASSDEN,
            bins = aqi_breaks,
            title = "Hourly PM₂.₅ (µg/m³)",
            position = "bottomright"
          )
      }
      
      # Other Variable Trends
      else if (var_inp() == "VENT_RATE") {
        req(var_inp(), fcst_hour())
        
        file_path <- paste0("data/", var_inp(), "/", var_inp(), "_", today, ".tif")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Hourly data not available for", today, "model run"))
          return()
        }
        
        
        hourly_rast <- rast(file_path)
        hourly_rast_selected <- hourly_rast[[fcst_hour() + 1]]
        
        palette <- get(paste0(var_inp(), "_palette"))
        
        map_proxy %>%
          addRasterImage(hourly_rast_selected, colors = palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                           padding: 4px 8px; border-radius: 4px;
                           font-size: 14px;'><strong>Forecast Time:</strong> ",
                                   forecast_label, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = palette,
            values = values(hourly_rast_selected),  # Ensure legend values match selected layer
            title = HTML(paste0(get_label(var_inp()))),
            position = "bottomright",
            labFormat = function(type, cuts, p) {
              VENT_RATE_labels
            }
          )
      }
      
      # Other Variable Trends
      else {
        req(var_inp(), fcst_hour())
        
        file_path <- paste0("data/", var_inp(), "/", var_inp(), "_", today, ".tif")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Hourly data not available for", today, "model run"))
          return()
        }
        
        
        hourly_rast <- rast(file_path)
        hourly_rast_selected <- hourly_rast[[fcst_hour() + 1]]
        
        palette <- get(paste0(var_inp(), "_palette"))
        
        map_proxy %>%
          addRasterImage(hourly_rast_selected, colors = palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                           padding: 4px 8px; border-radius: 4px;
                           font-size: 14px;'><strong>Forecast Time:</strong> ",
                                   forecast_label, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = palette,
            values = values(hourly_rast_selected),  # Ensure legend values match selected layer
            title = HTML(paste0(get_label(var_inp()))),
            position = "bottomright"
          )
      }
    })
  })
}
