hourly_map_ModuleUI <- function(id) {
  ns <- NS(id)
  
  leafletOutput(ns("map"), height = 700)
}



hourly_map_ModuleServer <- function(id, today, offset_hours, var_inp, layer, speed, playing) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns   
    
    today_str <- format(today, "%Y-%m-%d")  # <--- format once
    
    # --- Render map ---
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        fitBounds(montana_bounds[[1]][2], montana_bounds[[1]][1],
                  montana_bounds[[2]][2], montana_bounds[[2]][1]) %>%
        addPolygons(
          data = mt_counties,
          fill = TRUE,
          fillOpacity = 0,
          color = "black",
          weight = 0.5,
          opacity = 1,
          popup = ~paste0("<strong>", NAME, " County</strong><br>")
        ) %>%
        add_airnow_layers(airnow_data) %>%
        add_fire_layers(perim_data_sf, point_data_sf) %>%
        addLayersControl(
          overlayGroups = c(
            "Wildfires (perimeters)",
            "Wildfires (points)",
            "Monitors"
          ),
          options = layersControlOptions(collapsed = FALSE),
          position = "bottomleft"
        ) %>%
        onRender(sprintf("
          function(el, x) {
            var map = this;
            window.map = map;
            var bounds = [[%f, %f], [%f, %f]];
            window.imageLayers = [];
            for (var i=1; i<=48; i++) {
              var img = L.imageOverlay('MASSDEN/%s_' + String(i).padStart(2, '0') + '.png', bounds, {opacity: 0});
              img.addTo(map);
              window.imageLayers.push(img);
            }
            window.imageLayers[0].setOpacity(0.8);
            Shiny.setInputValue('%s', true, {priority: 'event'});
          }
        ",
                         montana_bounds[[1]][1], montana_bounds[[1]][2],
                         montana_bounds[[2]][1], montana_bounds[[2]][2],
                         today_str,
                         ns("map_rendered"))) %>%
        setView(lng = -110.0, lat = 47.0, zoom = 7)
    })
    
    # --- Initial datetime & legend once after map load ---
    observeEvent(input$map_rendered, {
      forecast_datetime <- as.POSIXct(as.Date(today)) +
        lubridate::hours(layer()) - lubridate::hours(offset_hours)
      
      leafletProxy("map", session) %>%
        removeControl("forecastTime") %>%
        addControl(
          html = paste0(
            "<div style='background: rgba(255,255,255,0.8);
              padding: 4px 8px; border-radius: 4px;
              font-size: 14px;'><strong>Forecast Date:</strong> ",
            format(forecast_datetime, "%Y-%m-%d %H:%M"), "</div>"
          ),
          position = "topright",
          layerId = "forecastTime"
        ) %>%
        removeControl("varLegend") %>%
        add_var_legend(var_inp())
    }, once = TRUE)
    
    # --- JS handler for dynamic code ---
    insertUI(selector = "body", ui = tags$script(HTML("
      Shiny.addCustomMessageHandler('jsCode', function(message) {
        eval(message.code);
      });
    ")))
    
    # --- Change variable: update image overlays only ---
    observeEvent(var_inp(), {
      js_code <- sprintf("
        if(window.imageLayers){
          window.imageLayers.forEach(function(layer){ layer.remove(); });
        }
        window.imageLayers = [];
        var bounds = [[%f, %f], [%f, %f]];
        for (var i=1; i<=48; i++) {
          var img = L.imageOverlay('%s/%s_' + String(i).padStart(2, '0') + '.png', bounds, {opacity: 0});
          img.addTo(window.map);
          window.imageLayers.push(img);
        }
        window.imageLayers[0].setOpacity(0.8);
      ",
                         montana_bounds[[1]][1], montana_bounds[[1]][2],
                         montana_bounds[[2]][1], montana_bounds[[2]][2],
                         var_inp(),
                         today_str)
      session$sendCustomMessage("jsCode", list(code = js_code))
      
      leafletProxy("map", session) %>%
        removeControl("varLegend") %>%
        add_var_legend(var_inp())
    })
    
    # --- Play / Pause animation ---
    observe({
      if (playing()) {
        invalidateLater(speed(), session)
        isolate({
          new_layer <- ifelse(layer() >= 48, 1, layer() + 1)
          updateSliderInput(session$rootScope(), "layer", value = new_layer)
        })
      }
    })
    
    # --- Switch forecast hour layer ---
    observeEvent(layer(), {
      js_code <- sprintf("
        if(window.imageLayers){
          window.imageLayers.forEach(function(layer){layer.setOpacity(0);});
          window.imageLayers[%d - 1].setOpacity(0.8);
        }
      ", layer())
      session$sendCustomMessage("jsCode", list(code = js_code))
      
      forecast_datetime <- as.POSIXct(as.Date(today)) + 
        lubridate::hours(layer()) - lubridate::hours(offset_hours)
      
      leafletProxy("map", session) %>%
        removeControl("forecastTime") %>%
        addControl(
          html = paste0(
            "<div style='background: rgba(255,255,255,0.8);
              padding: 4px 8px; border-radius: 4px;
              font-size: 14px;'><strong>Forecast Date:</strong> ",
            format(forecast_datetime, "%Y-%m-%d %H:%M"), "</div>"
          ),
          position = "topright",
          layerId = "forecastTime"
        )
    })
  })
}




