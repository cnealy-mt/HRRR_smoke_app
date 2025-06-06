outlook_map_ModuleUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("outlook_map"), height = 600)
}


outlook_map_ModuleServer <- function(id, today, var_inp, vent_category, trend_duration, transparency, active_tab) {
  moduleServer(id, function(input, output, session) {
    today <- as.Date(today)
    forecast_date <- today + 1
    
    # 1️⃣ Render empty base map once
    observeEvent(active_tab(), { #added observeEvent and invalidateLater delay to make sure Leaflet doesn't break when switching back and forth between map tabs...Leaflet container struggles with being hidden and re-rendering
      if (active_tab() == "Outlook and Trends") {
        invalidateLater(300, session)
        output$outlook_map <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            setView(lng = -110.0, lat = 47.0, zoom = 7)
          })
        }
    })
    
    # 2️⃣ Observe input changes and update map layers
    observe({
      req(active_tab() == "Outlook and Trends")
      req(var_inp(), transparency())
      
      map_proxy <- leafletProxy("outlook_map", session)
      
      # Clear old layers
      map_proxy %>% clearShapes() %>% clearImages() %>% clearControls()
      
      # AQI 24hr
      if (var_inp() == "AQI_24hr") {
        file_path <- paste0("data/county_24hr_avg/", today, "_county_24hr_avg.rds")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        county_24hr_avg <- readRDS(file_path)
        
        mt_counties_AQI <- mt_counties %>%
          left_join(county_24hr_avg, by = c("NAME" = "county"))
        
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
              "Smoke Concentration (24hr): ", round(MASSDEN, 1), " µg/m³<br>",
              "AQI (24hr): ", AQI_category
            )
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   forecast_date, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = smoke_pal,
            values = mt_counties_AQI$MASSDEN,
            bins = aqi_breaks,
            title = "24-Hr PM₂.₅ (µg/m³)",
            position = "bottomright"
          )
        
      }
      
      # AQI 24hr
      else if (var_inp() == "AQI_today_update") {
        file_path <- paste0("data/county_24hr_avg/", today, "_updated_today_AQI_outlook.rds")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        county_18hr_avg <- readRDS(file_path)
        
        mt_counties_AQI <- mt_counties %>%
          left_join(county_18hr_avg, by = c("NAME" = "county"))
        
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
              "Smoke Concentration (6am-12am): ", round(MASSDEN, 1), " µg/m³<br>",
              "AQI (18hr): ", AQI_category
            )
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = smoke_pal,
            values = mt_counties_AQI$MASSDEN,
            bins = aqi_breaks,
            title = "18-Hr PM₂.₅ (µg/m³)",
            position = "bottomright"
          )
        
      }
      
      else if (var_inp() == "VENT_RATE_max") {
        file_path <- paste0("data/county_24hr_avg/", today, "_county_24hr_avg.rds")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        vent_rate_max <- readRDS(file_path)
        
        mt_counties_VENT_RATE_max <- mt_counties %>%
          left_join(vent_rate_max, by = c("NAME" = "county"))
        
        
        map_proxy %>%
          addPolygons(
            data = mt_counties_VENT_RATE_max,
            fillColor = ~VENT_RATE_palette(VENT_RATE_max),
            fillOpacity = as.numeric(transparency()),
            color = "black",
            weight = .5,
            popup = ~paste0(
              "<strong>", NAME, " County</strong><br>",
              "24-hr Max Vent. Rate: ", round(VENT_RATE_max, 1), " m²/s"
            )
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   forecast_date, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_RATE_palette,
            values = mt_counties_VENT_RATE_max$VENT_RATE_max,
            bins = aqi_breaks,
            title = paste0(get_label(var_inp())),
            position = "bottomright",
            labFormat = function(type, cuts, p) {
              VENT_RATE_labels
            }
          )
      }
      
      else if (var_inp() == "VENT_RATE_max_today_update") {
        file_path <- paste0("data/county_24hr_avg/", today, "_updated_today_AQI_outlook.rds")
        
        if (!file.exists(file_path)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        vent_rate_max <- readRDS(file_path)
        
        mt_counties_VENT_RATE_max <- mt_counties %>%
          left_join(vent_rate_max, by = c("NAME" = "county"))
        
        
        map_proxy %>%
          addPolygons(
            data = mt_counties_VENT_RATE_max,
            fillColor = ~VENT_RATE_palette(VENT_RATE_max),
            fillOpacity = as.numeric(transparency()),
            color = "black",
            weight = .5,
            popup = ~paste0(
              "<strong>", NAME, " County</strong><br>",
              "24-hr Max Vent. Rate: ", round(VENT_RATE_max, 1), " m²/s"
            )
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_RATE_palette,
            values = mt_counties_VENT_RATE_max$VENT_RATE_max,
            bins = aqi_breaks,
            title = paste0(get_label(var_inp())),
            position = "bottomright",
            labFormat = function(type, cuts, p) {
              VENT_RATE_labels
            }
          )
      }
      
      else if (var_inp() == "VENT_WINDOW_today_update") {
        req(vent_category())
        
        rds_file <- paste0("data/VENT_WINDOW/", today, "_VENT_WINDOW_", vent_category(), "_county_avg_today_update.rds")
        raster_file <- paste0("data/VENT_WINDOW/", today, "_", vent_category(), "_rast_today_update.tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        vent_rast <- rast(raster_file)
        vent_df <- readRDS(rds_file)
        
        mt_counties_VENT_WINDOW <- mt_counties %>%
          left_join(vent_df, by = c("NAME" = "county"))
        
        map_proxy %>%
          addRasterImage(vent_rast, colors = VENT_WINDOW_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties_VENT_WINDOW,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>",
                            vent_category()," Hours: ", round(VENT_WINDOW, 1))
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_WINDOW_palette,
            values = values(vent_rast),  # Use domain directly for legend
            title = paste0(get_label(var_inp()), "<br><small>Trend</small>"),
            position = "bottomright"
          )
      }
      
      else if (var_inp() == "VENT_WINDOW") {
        req(vent_category())
        
        rds_file <- paste0("data/VENT_WINDOW/", today, "_VENT_WINDOW_", vent_category(), "_county_avg.rds")
        raster_file <- paste0("data/VENT_WINDOW/", today, "_", vent_category(), "_rast.tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        vent_rast <- rast(raster_file)
        vent_df <- readRDS(rds_file)
        
        mt_counties_VENT_WINDOW <- mt_counties %>%
          left_join(vent_df, by = c("NAME" = "county"))
        
        map_proxy %>%
          addRasterImage(vent_rast, colors = VENT_WINDOW_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties_VENT_WINDOW,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>",
                            vent_category()," Hours: ", round(VENT_WINDOW, 1))
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   forecast_date, "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_WINDOW_palette,
            values = values(vent_rast),  # Use domain directly for legend
            title = paste0(get_label(var_inp()), "<br><small>Trend</small>"),
            position = "bottomright"
          )
      }
      
      # AQI Trend
      else if (var_inp() == "AQI_trend") {
        req(trend_duration())
        
        trend_file <- paste0("data/trend/county_trend_", trend_duration(), ".rds")
        
        if (!file.exists(trend_file)) {
          add_no_data_message(map_proxy, paste("Trend data not available for", today, "model run"))
          return()
        }
        
        county_trend <- readRDS(trend_file)
        
        mt_counties_AQI_trend <- mt_counties %>%
          left_join(county_trend, by = c("NAME" = "county"))
        
        map_proxy %>%
          addPolygons(
            data = mt_counties_AQI_trend,
            fillColor = ~aqi_trend_palette(AQI_HRRR_trend),
            fillOpacity = as.numeric(transparency()),
            color = "black",
            weight = 0.5,
            popup = ~paste0("<strong>", NAME, " County</strong><br>",
                            "AQI Category Change: ", AQI_HRRR_trend)
          ) %>%
          add_fire_layers(perim_data_sf, point_data_sf) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Trend:</strong> ",
                                   get_trend_label(trend_duration(), today), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = aqi_trend_palette,
            values = aqi_trend_values,
            title = HTML("<div style='width: 70px; word-wrap: break-word;'>AQI Category Change</div>"),
            position = "bottomright"
          )
      }
      
      else {
        req(trend_duration())
        
        # Load raster
        raster_file <- paste0("data/trend/", var_inp(), "_", trend_duration(), "_trend.tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Trend data not available for", today, "model run"))
          return()
        }
        
        trend_rast <- rast(raster_file)
        
        # Set domain dynamically
        domain_values <- if (var_inp() == "MASSDEN") c(-100, 100) else c(-max(values(trend_rast), na.rm = TRUE), max(values(trend_rast), na.rm = TRUE))
        
        # Apply squishing by modifying raster values
        values(trend_rast) <- squish(values(trend_rast), range = domain_values)
        
        # Define color palette
        trend_palette <- colorNumeric(
          palette = rev(brewer.pal(11, "RdBu")),
          domain = domain_values,
          na.color = "transparent"
        )
        
        # Now proceed with Leaflet mapping
        map_proxy %>%
          addRasterImage(trend_rast, colors = trend_palette, opacity = as.numeric(transparency())) %>%
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
                           font-size: 14px;'><strong>Trend:</strong> ",
                                   get_trend_label(trend_duration(), today), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = trend_palette,
            values = domain_values,  # Use domain directly for legend
            title = HTML(paste0(get_label(var_inp()), "<br><small>Trend</small>")),
            position = "bottomright"
          )
      }
    })
    
  })
}
