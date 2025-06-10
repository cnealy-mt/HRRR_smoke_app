library(terra)
library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(highcharter)
library(purrr)
library(tidyr)
library(lubridate)
library(glue)
library(stringr)
library(writexl)

get_latest_update_date <- function(dir_path = "data/county_24hr_avg") {
  files <- list.files(path = dir_path, pattern = "^\\d{4}-\\d{2}-\\d{2}_.+\\.rds$", full.names = FALSE)

  # Extract date part from filenames
  dates <- as.Date(sub("^(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", files))

  # Return the most recent date
  max(dates, na.rm = TRUE)
}

update_date <- get_latest_update_date()
# update_date <- "2024-07-25" #format(Sys.Date(), "%Y-%m-%d")

today <- update_date
airnow_today <- Sys.Date() #use Sys.Date() when operational or else AirNow hourly data won't plot properly between midnight and 8am (~model data update)


#modules
source(paste0("modules/utils/variable_utils.R"))
source(paste0("modules/utils/map_module_utils.R"))
source(paste0("modules/utils/plot_utils.R"))


source(paste0("modules/outlook_map_module.R"))
source(paste0("modules/hourly_map_module.R"))
source(paste0("modules/fire_table_module.R"))
source(paste0("modules/county_plot_module.R"))
source(paste0("modules/worm_plot_module.R"))
source(paste0("modules/model_performance_tile_module.R"))
source(paste0("modules/bias_plot_module.R"))
source(paste0("modules/model_performance_timeseries_module.R"))
source(paste0("modules/aqa_text_module.R"))






ui <- page_sidebar(
  tags$head(
    tags$style(HTML("
      .header-bar {
      background-color: #004A98;
      color: white;
      padding: 0 20px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }

    .header-bar img {
      height: 45px;
      width: auto;
      margin-top: 10px;
      margin-bottom: 10px;
      display: block;
    }

    .header-bar h1 {
      margin: 0;
      font-size: 24px;
    }
    "))
  ),
  
  div(class = "header-bar",
      img(src = "logo_blue.png", alt = "Logo"),
      h1("AQB Smoke Forecasting Dashboard")
  ),
  #-----------------------------------------------------------------------------------------
  #----------------------------------------SIDEBAR-----------------------------------------------------
  #-----------------------------------------------------------------------------------------
  
  sidebar = sidebar(
    width = "20%",
    position = "right",
    open = "always",
    
    #------------------------------------Outlook and Trends Tab-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Outlook and Trends'",
      selectInput("var_inp_outlook", "Variable:",
                choices = c(
                  "AQI Outlook (tomorrow)" = "AQI_24hr",
                  "AQI Outlook (today)" = "AQI_today_update",
                  "Ventilation Rate (tomorrow)" = "VENT_RATE_max",
                  "Ventilation Rate (today)" = "VENT_RATE_max_today_update",
                  "Ventilation Window (tomorrow)" = "VENT_WINDOW",
                  "Ventilation Window (today)" = "VENT_WINDOW_today_update",
                  "AQI Category (trend)" = "AQI_trend",
                  "PM2.5 (trend)" = "MASSDEN",
                  "Temperature (trend)" = "TMP",
                  "Precipitation (trend)" = "PRATE",
                  "Relative Humidity (trend)" = "RH",
                  "Wind Gust (trend)" = "GUST",
                  "Wind Speed (trend)" = "WIND_1hr_max_fcst",
                  "Boundary Layer Height (trend)" = "HPBL",
                  "Ventilation Rate (trend)" = "VENT_RATE"
                )
    )),
    
    conditionalPanel(
      condition = "(
          input.var_inp_outlook == 'VENT_WINDOW' || 
          input.var_inp_outlook == 'VENT_WINDOW_today_update'
          ) && input.main_tabs == 'Outlook and Trends'",
      selectInput("vent_category", "Ventilation Threshold:",
                  choices = c("Good or better" = "Good", "Marginal or better" = "Marginal"))
    )
    ,
    
    conditionalPanel(
      condition = "input.main_tabs == 'Outlook and Trends' && 
               input.var_inp_outlook != 'AQI_24hr' && 
               input.var_inp_outlook != 'AQI_today_update' && 
               input.var_inp_outlook != 'VENT_RATE_max' && 
               input.var_inp_outlook != 'VENT_RATE_max_today_update' &&
               input.var_inp_outlook != 'VENT_WINDOW' &&
               input.var_inp_outlook != 'VENT_WINDOW_today_update'",
      selectInput("trend_duration", "Trend Duration:",
                  choices = c("One Day" = "1day", "Two Day" = "2day"))
    )
    ,
    
    #------------------------------------Hourly Forecast Tab-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Hourly Forecast'",
      sliderInput("fcst_hour",
                  label = "Forecast Hour:",
                  min = 0,
                  max = 47,
                  value = 0,
                  step = 1,
                  animate = animationOptions(
                    interval = 1000,    # milliseconds between steps
                    loop = TRUE,        # whether to loop
                    playButton = NULL,  # use default play icon
                    pauseButton = NULL  # use default pause icon
                  )
      )
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Hourly Forecast'",
      selectInput("var_inp_hourly", "Variable:",
                  choices = c(
                    "AQI Category" = "AQI_hourly",
                    "PM2.5" = "MASSDEN",
                    "Temperature" = "TMP",
                    "Precipitation Rate" = "PRATE",
                    "Relative Humidity" = "RH",
                    "Wind Gust" = "GUST",
                    "Wind Speed" = "WIND_1hr_max_fcst",
                    "Boundary Layer Height" = "HPBL",
                    "Ventilation Rate" = "VENT_RATE"
                  )
      )),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Worm Plots'",
      selectInput("AirNow_select", "Hourly or Running Avg:",
                  choices = c(
                    "Hourly" = "hourly",
                    "24hr Running Average" = "running_avg"
                  ))),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Model Performance'",
      selectInput("lead_time", "Lead Time:",
                  choices = c(
                    "0 Days" = "zero_day",
                    "1 Day" = "one_day"
                  ))),
    
    
    
    
    #------------------------------------MISC Inputs-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs === 'Outlook and Trends' || input.main_tabs === 'Hourly Forecast'",
      selectInput("transparency", "Opacity:",
                choices = c("0%" = 0, "25%" = 0.25, "50%" = 0.5, "75%" = 0.75, "100%" = 1),
                selected = 0.75),
      fire_table_ModuleUI("fire_table") #fire table download button (has its own module file due to complexity)
      ),
    
    #------------------------------------Text-------------------------------------------
    conditionalPanel(
      condition = "input.main_tabs == 'Outlook and Trends'",
      p(""),
      h4("Outlook and Trends"),
      p("Use the 'variable' selector above to switch between AQI outlook, ventilation rate, and trend views. Use the selector below the map to view 48-hour meteorological timeseries for each Montana county."),
      
      h4("AQI Outlook"),
      p("The default view is tomorrow's forecasted AQI category outlook that employs a county-area 24-hour average of the HRRR near-surface smoke output. Counties are shaded based on their expected PM2.5-based 24-hour AQI category.", style = "margin-left: 20px;"),
      p("The second drop-down option is an updated AQI category outlook for today. This view averages near-surface smoke from 6am-12am (today's valid model hours) and provides a more confident prediction of conditions today.", style = "margin-left: 20px;"),
      img(src = "AQI_scale.png", width = "60%", alt = "AQI Color Scale", style = "margin-left: 20px;"),
      
      h4("Ventilation Rate"),
      p("The ventilation rate view shades counties by their daily maximum ventilation rate. Breaks are based on those from the University of Washington.", style = "margin-left: 20px;"),
      p("Select the ventilation window variable and another dropdown menu will appear. With these options, you can view the number of hours tomorrow that will be equal to or above either the 'good' or 'marginal' ventilation threshold.", style = "margin-left: 20px;"),
      
      h4("Trends"),
      p("The remaining options labeled with '(trend)' display the expected change in each of the available meteorological variables. Selecting the '1 day' trend will compare tomorrow to today. The '2 day' trend will compare tomorrow to yesterday.", style = "margin-left: 20px;"),
      
      h4("Fire Table"),
      p("Click on the 'Download' button above to save a table of current active wildfires. Fires are filtered to only include those in surrounding states/provinces greater than 5,000 acres or those in Montana greater than 100 acres.", style = "margin-left: 20px;"),
      
      p("All data is based on the HRRR 48-hour forecast output that is valid at 6 MDT/5 MST. All HRRR data in this app is updated daily ~8:10am.", style = "font-size: 90%; font-style: italic;")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Hourly Forecast'",
      p(""),
      h4("Hourly Forecast"),
      p("Use the 'variable' selector above to view all 48-hours of available data on the map. Either drag the time slider or click the play button to view different hours. Use the selector below the map to view 48-hour meteorological timeseries for each Montana county."),
      
      h4("Fire Table"),
      p("Click on the 'Download' button above to save a table of current active wildfires. Fires are filtered to only include those in surrounding states/provinces greater than 1,000 acres or those in Montana greater than 100 acres.", style = "margin-left: 20px;"),
      
      p("All data is based on the HRRR 48-hour forecast output that is valid at 6 MDT/5 MST. All HRRR data in this app is updated daily ~8:10am.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Worm Plots'",
      p(""),
      h4("Hourly Monitor Data"),
      p("This panel streams PM2.5 AirNow data from all active Montana DEQ monitors. Hover over lines to view time (MDT) and values."),
      
      p("Data updated hourly.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Model Performance'",
      p(""),
      h4("Model Performance"),
      p("Select 'lead time' above to view model performance statistics. The '0 day' lead time compares observed (monitored) values to model data produced on the day of monitor measurement. The '1 day' lead time compares values collected the day after the model forecast was produced."),
      
      h4("Bias Plot"),
      p("The bias plot only considers 'wildfire season' data from May through October. The HRRR model does not estimate anthropogenic emissions, and therefore emission estimates during the cooler months are systematically underestimated to a greater degree than the warm season.", style = "margin-left: 20px;"),
      
      p("Model performance data lags by one day.", style = "font-size: 90%; font-style: italic;")
      
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'AQA Text Product'",
      p(""),
      h4("Air Quality Alerts"),
      p("Create an AQA text file for coordination with the NWS by selecting the AQI outlook to use and clicking the download button. Expiration time defaults to 8AM. If the 'today' outlook is used, expiration date is set to tomorrow. If the 'tomorrow' outlook is used, expiration date is set to two days from now."),
      
      #p("Model performance data lags by one day.", style = "font-size: 90%; font-style: italic;")
      
    )
  ),
  #-----------------------------------------------------------------------------------------
  #---------------------------------------TABS---------------------------------------------
  #-----------------------------------------------------------------------------------------
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Outlook and Trends",
             # Map module output
             outlook_map_ModuleUI("outlook_map"),
             ),
    tabPanel("Hourly Forecast",
             hourly_map_ModuleUI("hourly_map")
             ),
    tabPanel("Worm Plots",
             worm_plot_ModuleUI("worm_plot")
    ),
    tabPanel("Model Performance"),
    tabPanel("AQA Text Product") # moduleUI in conditionalPanel below
  ),
  #-----------------------------------------------------------------------------------------
  #-------------------------------------MAIN PLOT AREA--------------------------------------
  #-----------------------------------------------------------------------------------------
  
  #------------------------------------County Timeseries-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs === 'Outlook and Trends' || input.main_tabs === 'Hourly Forecast'",       # Show plot only under these two tabs
    selectInput(
      inputId = "county",
      label = "Select County Forecast",
      choices = montana_counties, #from plot_utils.R
      selected = "Lewis and Clark"
    ),
    county_plot_ModuleUI("county_plot")
  ),
  
  #------------------------------------Model Performance-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs == 'Model Performance'",
    
    # Inputs row (3 inputs in one row)
    fluidRow(
      column(
        width = 4,
        selectInput("year_select", "Year:",
                    choices = seq(as.numeric(format(Sys.Date(), "%Y")) - 4,
                                    as.numeric(format(Sys.Date(), "%Y")))
                    ,
                    selected = as.numeric(format(Sys.Date(), "%Y")))
      ),
      column(
        width = 4,
        selectInput("month_select", "Month:",
                    choices = setNames(1:12, month.name),
                    selected = as.numeric(format(Sys.Date(), "%m")))
      ),
      column(
        width = 4,
        selectInput("tile_var", "Variable:",
                    choices = c(
                      "HRRR vs. Obs AQI" = "accuracy",
                      "HRRR Concentration" = "avg_HRRR_ug_m3",
                      "Obs Concentration" = "avg_sample_measurement"
                    ))
      )
    ),
    
    # First plot (model performance tile)
    div(class = "py-3", model_performance_tile_ModuleUI("tile_plot")),
    
    # Second section: model performance time series and bias plot side by side
    fluidRow(
      # Left column: model performance time series and site/date inputs
      column(
        width = 6,
        
        # Inputs ABOVE the time series plot
        fluidRow(
          column(
            width = 6,
            selectInput("site_name_mp", "Monitoring Site:",
                        choices = site_names)
          ),
          column(
            width = 6,
            dateRangeInput(
              inputId = "date_range_mp",
              label = "Select Date Range:",
              start = max(daily_model_performance$date),
              end = max(daily_model_performance$date),
              min = min(daily_model_performance$date),
              max = max(daily_model_performance$date),
              format = "yyyy-mm-dd",
              startview = "month"
            )
          )
        ),
        
        # Time series plot underneath
        model_performance_timeseries_ModuleUI("model_performance_timeseries")
      ),
      
      # Right column: bias plot + year selector above
      column(
        width = 6,
        
        # Year selector ABOVE the bias plot
        selectInput(
          inputId = "bias_plot_year",
          label = "Select Year:",
          choices = seq(as.numeric(format(Sys.Date(), "%Y")) - 4,
                        as.numeric(format(Sys.Date(), "%Y"))),
          selected = as.numeric(format(Sys.Date(), "%Y"))
        ),
        
        # Bias plot underneath
        bias_plot_ModuleUI("bias_plot")
      )
    )
  ),
  
  #------------------------------------AQA Tab-------------------------------------------
  conditionalPanel(
    condition = "input.main_tabs == 'AQA Text Product'",
    
    # Add "Use:" selection
    div(
      style = "margin-bottom: 10px;",
      radioButtons(
        inputId = "aqi_outlook_choice",
        label = "Use:",
        choices = c("Today AQI Outlook", "Tomorrow AQI Outlook"),
        selected = "Today AQI Outlook",
        inline = TRUE
      )
    ),
    
    # Flex container for two inputs side-by-side, tightly spaced
    div(
      style = "display: flex; gap: 10px; align-items: center;",
      textInput("exp_time", "Expiration Time", value = "8AM", width = "150px"),
      dateInput("exp_date", "Expiration Date", value = Sys.Date(), width = "200px")
    ),
    
    # Full width reason input with width: 100%
    fluidRow(
      column(
        12,
        textInput(
          "reason", 
          "Reason for alert:", 
          placeholder = "Optional: brief description of AQ & smoke conditions",
          width = "100%"
        )
      )
    ),
    
    aqa_text_ModuleUI("aqa_message")
  )
  
)




server <- function(input, output, session) {
  outlook_map_ModuleServer(
    id = "outlook_map",
    today = today,
    var_inp = reactive(input$var_inp_outlook),
    vent_category = reactive(input$vent_category),
    trend_duration = reactive(input$trend_duration),
    transparency = reactive(input$transparency),
    active_tab = reactive(input$main_tabs)
  )
  
  hourly_map_ModuleServer(
    id = "hourly_map",
    today = today,
    var_inp = reactive(input$var_inp_hourly),
    fcst_hour = reactive(input$fcst_hour),
    transparency = reactive(input$transparency),
    active_tab = reactive(input$main_tabs)  # NEW
  )
  
  fire_table_ModuleServer("fire_table")
  
  county_plot_ModuleServer(
    id = "county_plot",  # Same ID as in county_plot_UI
    today = today,
    county = reactive(input$county)  # From the global selectInput
  )
  
  worm_plot_ModuleServer(
    id = "worm_plot",
    airnow_today = airnow_today,
    AirNow_select = reactive(input$AirNow_select)
  )
  
  observeEvent(input$lead_time, {
    if (input$lead_time == "zero_day") {
      updateSelectInput(
        session,
        inputId = "tile_var",
        choices = c(
          "HRRR vs. Obs AQI" = "accuracy_update",
          "HRRR Concentration" = "avg_HRRR_ug_m3_today_update",
          "Obs Concentration" = "avg_sample_measurement"
        )
      )
    } else if (input$lead_time == "one_day") {
      updateSelectInput(
        session,
        inputId = "tile_var",
        choices = c(
          "HRRR vs. Obs AQI" = "accuracy",
          "HRRR Concentration" = "avg_HRRR_ug_m3",
          "Obs Concentration" = "avg_sample_measurement"
        )
      )
    }
  })
  
  
  model_performance_tile_ModuleServer(
    id = "tile_plot",
    year = reactive(input$year_select),
    month = reactive(input$month_select),
    variable = reactive(input$tile_var)
  )
  
  bias_plot_ModuleServer(
    id = "bias_plot",
    lead_time = reactive(input$lead_time),
    year = reactive(input$bias_plot_year)
  )
  
  model_performance_timeseries_ModuleServer(
    id = "model_performance_timeseries",
    site_name_mp = reactive(input$site_name_mp),
    start_date_mp = reactive(input$date_range_mp[1]),
    end_date_mp = reactive(input$date_range_mp[2]),
    lead_time = reactive(input$lead_time)
  )
  
  observeEvent(input$aqi_outlook_choice, {
    new_date <- if (input$aqi_outlook_choice == "Today AQI Outlook") {
      Sys.Date() + 1
    } else {
      Sys.Date() + 2
    }
    
    updateDateInput(
      session,
      inputId = "exp_date",
      value = new_date
    )
  })
  
  aqa_text_ModuleServer(
    id = "aqa_message",
    today = today,
    airnow_today = airnow_today,
    aqi_outlook_choice = reactive(input$aqi_outlook_choice),
    exp_time = reactive(input$exp_time),
    exp_date = reactive(input$exp_date),
    reason = reactive(input$reason)
  )
}


shinyApp(ui, server)
