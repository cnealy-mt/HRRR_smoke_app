# app.R
library(shiny)
library(leaflet)
library(dplyr)
library(readxl)
library(here)
library(bslib)
library(purrr)
library(DT)
library(zoo)
library(slider)
library(tidyr)

here::i_am("app.R")

# Source utilities
source(here("modules","plotting_functions","utilities","get_NAAQS.R"))
source(here("modules","plotting_functions","utilities","load_map_data.R"))
source(here("modules","plotting_functions", "render_map.R"))
source(here("modules","plotting_functions", "utilities", "aqi_helpers.R"))
source(here("modules","plotting_functions", "utilities", "DV_calc_module_funcs.R"))


# Source modules
source(here("modules","map_module.R"))
source(here("modules","historical_DV_module.R"))
source(here("modules","DV_bar_module.R"))
source(here("modules","daily_values_module.R"))
source(here("modules","daily_tile_module.R"))
source(here("modules","worm_plot_module.R"))
source(here("modules","cumulative_PM25.R"))
source(here("modules","DV_calc_module.R"))


ui <- page_sidebar(
  # Adding custom styles to style the header bar
  tags$head(
    tags$style(HTML("
      .header-bar {
      background-color: #004A98;
      color: white;
      padding: 0 20px; /* top-bottom: 0px, left-right: 20px */
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
  
  # Header bar with title and logo
  div(class = "header-bar",
      img(src = "logo_blue.png", alt = "Logo"),  # Path to image in www/
      h1("AQB DV Monitoring Tool")
  ),
  
  sidebar = sidebar(
    position = "right",
    open = "always",
    
    # Conditional sidebar content for Historical Data tab
    conditionalPanel(
      condition = "input.main_tabs == 'Historical Data'",
      
      # Input controls placed in the sidebar only for Historical Data tab
      h4("Parameters"),
      selectInput("pollutant", "Pollutant", 
                  choices = c("PM2.5" = "PM25", "PM10" = "PM10"), 
                  selected = "PM25"),
      selectInput("avg_period", "Averaging Period", 
                  choices = c("Annual" = "ann", "24 Hour" = "24hr"), 
                  selected = "ann"),
      numericInput("year_filter", "Year", 
                   value = as.numeric(format(Sys.Date()-1, "%Y")), 
                   min = 2000, max = as.numeric(format(Sys.Date()-1, "%Y")), step = 1),
      # 🧠 Conditionally show this ONLY when PM10 + 24hr is selected
      conditionalPanel(
        condition = "input.pollutant == 'PM10' && input.avg_period == '24hr'",
        selectInput("dv_type", "Design Value Type",
                    choices = c("NAAQS DV" = "NAAQS", "LMP DV" = "LMP"),
                    selected = "NAAQS")
      ),
      tags$hr(),
      
      # Additional context or info
      h4("About"),
      p("Click on a site from the map to see its design value history and 24-hr historical concentrations. Data are current through yesterday. Sites with a 
        thick red outline have a design value (with all wildfire data included) that exceeds the NAAQS for the year selected. Selecting older years will reveal sites and POCs that may no longer be operating."),
      p("Red vertical bands in the daily timeseries indicate wildfire-flagged data."),
      p("When data is not available from AQS, AirNow data is substituted."),
      tags$hr(),
      HTML("<strong>Note:</strong><br>Daily data are colored using discrete (timeseries) and continuous (tile plot) pollutant-specific AQI color scales."),
      br(),
      img(src = "AQI_scale.png", width = "75%", alt = "AQI Color Scale")
    ),
    
    # Conditional sidebar content for other tabs
    conditionalPanel(
      condition = "input.main_tabs == 'Cumulative PM2.5 DV Tracker'",
      h4("About"),
      p("Choose a current monitoring site from the drop-down menu. The red-orange line represents the current annual PM2.5 design value calculated from the previous 2 annual averages and the current-year average. 
        All other lines (other than the NAAQS) represent the cumulative average PM2.5 concentration."),
      HTML("<strong>Note:</strong> Annual averages are calculated by averaging quarterly averages. As a result, you may see odd 'jumps' in the DV tracker as the current year quarterly averages are established.")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == \"'Worm' Plot\"",
      h4("About"),
      p(HTML("Each plot represents measured hourly concentrations from the 3 previous days. Concentrations in &mu;g/m&sup3; are plotted on the y-axis. All data are from AirNow.")),      
      HTML("<strong>Tip:</strong> Hover over the lines to see values and time of day."),
      HTML("<strong>AQI Scale:</strong>"),
      img(src = "AQI_scale.png", width = "75%", alt = "AQI Color Scale")
    ),
    conditionalPanel(
      condition = "input.main_tabs == 'EE DV Calculator'",
      h4("About"),
      p("Choose pollutant, averaging period, DV year, and monitoring site to see a list of wildfire-flagged data. Click on individual days (or hold shift and click for multiple selection)
        to exclude those data from the design value calculation."),
      p("The resulting table at the bottom of the page lists the days selected, their concentration, and the resulting cumulative design value reduction. If no value populates in 'DV After Exclusion', then the
        selected site doesn't have enough data to calculate the DV."),
      p("Click the 'download' button to save a CSV file of the results."),
      HTML("<strong>Note:</strong> The cumulative DV reduction is calculated in order of the days that are selected for exclusion. For example, it's possible to exclude data in descending order of concentration
           or in chronological order.")
    )
  ),
  
  # Main content
  tabsetPanel(
    id = "main_tabs",  # Important for conditionalPanel to work
    tabPanel("Historical Data",
             mapUI("main_map"),
             # The input selections are already in the sidebar for this tab
             DV_bar_UI("dv_bar"),
             historical_DVUI("dv_history"),
             historical_daily_UI("daily_values"),
             historical_tile_UI("daily_tile_plot")
    ),
    tabPanel("EE DV Calculator",
             design_value_module_ui("design_value_module")),
    
    tabPanel("Cumulative PM2.5 DV Tracker", 
             cumulative_PM25UI("cumulative_plot")),
    
    tabPanel("'Worm' Plot", 
             worm_plot_UI("worm_plots"))
  )
)




# Server
server <- function(input, output, session) {
  
  # Create a shared reactiveVal to hold the selected site
  selected_site <- reactiveVal(NULL)
  
  map_result <- mapServer(
    "main_map",
    pollutant = reactive(input$pollutant),
    avg_period = reactive(input$avg_period),
    year_filter = reactive(input$year_filter),
    selected_site = selected_site  # allow map to update selected_site
  )
  
  designValueBarServer(
    id = "dv_bar",
    pollutant = reactive(input$pollutant),
    avg_period = reactive(input$avg_period),
    year_filter = reactive(input$year_filter),
    dv_type = reactive(input$dv_type)
  )
  
  historical_DVServer(
    "dv_history",
    selected_site = selected_site,  # read the most recent selected site (map or bar)
    pollutant = reactive(input$pollutant),
    avg_period = reactive(input$avg_period),
    year_filter = reactive(input$year_filter),
    dv_type = reactive(input$dv_type)
  )
  
  historical_daily_Server(
    "daily_values",
    selected_site = selected_site,
    pollutant = reactive(input$pollutant),
    year_filter = reactive(input$year_filter)
  )
  
  historical_tile_Server(
    "daily_tile_plot",
    selected_site = selected_site,
    pollutant = reactive(input$pollutant),
    year_filter = reactive(input$year_filter)
  )
  
  # Call the cumulative PM25 module
  cumulative_PM25Server(
    id = "cumulative_plot"
  )
  
  # Pass the required parameters to the worm_plot_Server module
  worm_plot_Server(
    "worm_plots", 
    year_filter = year(Sys.Date() - 1),
    start = Sys.Date() - 3,
    end = Sys.Date() - 1
  )
  
  design_value_module_server("design_value_module")
  
}

# Run the app
shinyApp(ui, server)
