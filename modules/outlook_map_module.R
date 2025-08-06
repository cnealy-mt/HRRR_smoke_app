outlook_map_ModuleUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("outlook_map"), height = 700)
}


outlook_map_ModuleServer <- function(id, today, var_inp, lead_time, vent_category, transparency, active_tab) {
  moduleServer(id, function(input, output, session) {
    today <- as.Date(today)

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
      req(var_inp(), lead_time(), transparency())
      
      map_proxy <- leafletProxy("outlook_map", session)
      
      # Clear old layers
      map_proxy %>% clearShapes() %>% clearImages() %>% clearControls()
      
      
      #===========================County Polygon Avg/Max Maps=======================
      # AQI County Avg
      if (var_inp() == "AQI_cty") {
        file_path <- paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead", lead_time(), ".rds")
        
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
            layerId = ~NAME,
            popup = ~paste0(
              "<strong>", NAME, " County</strong><br>",
              "Smoke Concentration (24hr): ", round(MASSDEN, 1), " µg/m³<br>",
              "AQI: ", AQI_category
            )
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = smoke_pal,
            values = mt_counties_AQI$MASSDEN,
            bins = aqi_breaks,
            title = "24-Hr PM₂.₅ (µg/m³)",
            position = "bottomright"
          )
        
      }
      
      # County Vent Rate Max
      else if (var_inp() == "VENT_RATE_max") {
        file_path <- paste0("data/county_24hr_avg/", today, "_county_24hr_avg_lead", lead_time(), ".rds")
        
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
            layerId = ~NAME,
            popup = ~paste0(
              "<strong>", NAME, " County</strong><br>",
              "24-hr Max Vent. Rate: ", round(VENT_RATE_max, 1), " m²/s"
            )
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_RATE_palette,
            values = mt_counties_VENT_RATE_max$VENT_RATE_max,
            title = paste0(get_label(var_inp())),
            position = "bottomright",
            labFormat = function(type, cuts, p) {
              VENT_RATE_labels
            }
          )
      }
      #============================Vent Window Raster w/marginal/good selection=====================
      # Raster Vent Window
      else if (var_inp() == "VENT_WINDOW") {
        req(vent_category())
        
        raster_file <- paste0("data/VENT_WINDOW/", today, "_", vent_category(), "_rast_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        vent_rast <- rast(raster_file)

        map_proxy %>%
          addRasterImage(vent_rast, colors = VENT_WINDOW_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = VENT_WINDOW_palette,
            values = values(vent_rast),  # Use domain directly for legend
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      #==============================Rasters With Transparency=============================
      # MASSDEN_avg
      else if (var_inp() == "MASSDEN_avg") {

        raster_file <- paste0("data/MASSDEN/", today, "_MASSDEN_avg_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- colorBin(
          palette = MASSDEN_colors,
          bins = MASSDEN_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        vals <- values(rast)
        vals[vals < 1] <- NA  # make <1 transparent
        
        map_proxy <- map_proxy %>%
          addRasterImage(rast, colors = MASSDEN_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # MASSDEN_max
      else if (var_inp() == "MASSDEN_max") {

        raster_file <- paste0("data/MASSDEN/", today, "_MASSDEN_max_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- colorBin(
          palette = MASSDEN_colors,
          bins = MASSDEN_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        vals <- values(rast)
        vals[vals < 1] <- NA  # make <1 transparent

        map_proxy <- map_proxy %>%
          addRasterImage(rast, colors = MASSDEN_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # PRATE_acc
      else if (var_inp() == "PRATE_acc") {

        raster_file <- paste0("data/PRATE/", today, "_PRATE_acc_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- colorBin(
          palette = PRATE_colors,
          bins = PRATE_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        vals <- values(rast)
        vals[vals < 0.00125] <- NA  # make <0.00125 transparent
        
        map_proxy <- map_proxy %>%
          addRasterImage(rast, colors = PRATE_palette, opacity = as.numeric(transparency())) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(html = paste0("<div style='background: rgba(255,255,255,0.8);
                                   padding: 4px 8px; border-radius: 4px;
                                   font-size: 14px;'><strong>Forecast Date:</strong> ",
                                   today + as.numeric(lead_time()), "</div>"),
                     position = "topright") %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      #==================================Rasters w/o Transparency=============================
      # AQI_avg
      else if (var_inp() == "AQI_avg") {

        raster_file <- paste0("data/MASSDEN/", today, "_MASSDEN_avg_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- colorBin(
          palette = aqi_colors,
          bins = aqi_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        vals <- values(rast)  # keep all values as-is
        
        map_proxy <- map_proxy %>%
          addRasterImage(
            rast,
            colors = legend_palette,  # directly use AQI palette
            opacity = as.numeric(transparency())
          ) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(
            html = paste0(
              "<div style='background: rgba(255,255,255,0.8);
                   padding: 4px 8px; border-radius: 4px;
                   font-size: 14px;'><strong>Forecast Date:</strong> ",
              today + as.numeric(lead_time()), "</div>"
            ),
            position = "topright"
          ) %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # AQI_max
      else if (var_inp() == "AQI_max") {
        
        raster_file <- paste0("data/MASSDEN/", today, "_MASSDEN_max_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- colorBin(
          palette = aqi_colors,
          bins = aqi_breaks,
          na.color = "transparent",
          right = FALSE
        )
        
        vals <- values(rast)  # keep all values as-is
        
        map_proxy <- map_proxy %>%
          addRasterImage(
            rast,
            colors = legend_palette,  # directly use AQI palette
            opacity = as.numeric(transparency())
          ) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(
            html = paste0(
              "<div style='background: rgba(255,255,255,0.8);
                   padding: 4px 8px; border-radius: 4px;
                   font-size: 14px;'><strong>Forecast Date:</strong> ",
              today + as.numeric(lead_time()), "</div>"
            ),
            position = "topright"
          ) %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # TMP_max
      else if (var_inp() == "TMP_max") {
        
        raster_file <- paste0("data/TMP/", today, "_TMP_max_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- TMP_palette
        
        vals <- values(rast)  # keep all values as-is
        
        map_proxy <- map_proxy %>%
          addRasterImage(
            rast,
            colors = legend_palette,  # directly use TMP palette
            opacity = as.numeric(transparency())
          ) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(
            html = paste0(
              "<div style='background: rgba(255,255,255,0.8);
                   padding: 4px 8px; border-radius: 4px;
                   font-size: 14px;'><strong>Forecast Date:</strong> ",
              today + as.numeric(lead_time()), "</div>"
            ),
            position = "topright"
          ) %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # WIND_1hr_max_fcst_max
      else if (var_inp() == "WIND_1hr_max_fcst_max") {
        
        raster_file <- paste0("data/WIND_1hr_max_fcst/", today, "_WIND_1hr_max_fcst_max_lead", lead_time(), ".tif")
        
        if (!file.exists(raster_file)) {
          add_no_data_message(map_proxy, paste("Outlook data not available for", today, "model run"))
          return()
        }
        
        rast <- rast(raster_file)
        
        legend_palette <- WIND_1hr_max_fcst_palette
        
        vals <- values(rast)  # keep all values as-is
        
        map_proxy <- map_proxy %>%
          addRasterImage(
            rast,
            colors = legend_palette,  # directly use WIND_1hr_max_fcst palette
            opacity = as.numeric(transparency())
          ) %>%
          addPolygons(
            data = mt_counties,
            fill = TRUE,
            fillOpacity = 0,
            color = "black",
            weight = .5,
            layerId = ~NAME,
            opacity = 1,
            popup = ~paste0("<strong>", NAME, " County</strong><br>")
          ) %>%
          addControl(
            html = paste0(
              "<div style='background: rgba(255,255,255,0.8);
                   padding: 4px 8px; border-radius: 4px;
                   font-size: 14px;'><strong>Forecast Date:</strong> ",
              today + as.numeric(lead_time()), "</div>"
            ),
            position = "topright"
          ) %>%
          addLegend(
            pal = legend_palette,
            values = vals,
            title = paste0(get_label(var_inp())),
            position = "bottomright"
          )
      }
      
      # add monitors and fire locs
      map_proxy <- add_airnow_layers(map_proxy, airnow_data)
      add_fire_layers(map_proxy, perim_data_sf, point_data_sf)
      
      # add control box
      map_proxy <- map_proxy %>%
        addLayersControl(
          overlayGroups = c(
            "Wildfires (perimeters)",
            "Wildfires (points)",
            "Monitors"
          ),
          options = layersControlOptions(collapsed = FALSE),
          position = "bottomleft"
        )
        
      
    })
    
  })
}
