#####################################################
# Dashboard app for heavy throws data visualisation #
#####################################################

library(tidyverse)
library(DescTools)
library(fpp3)
library(imputeTS)
library(bruceR)
library(shinydashboard)
library(slickR)

# load plotting functions
source("plottingFunctions.R")

# get men's discus finalists for forecasting
MDT_aths <- get_athletes(event = "Discus Throw",
                         gender = "Male",
                         EC_final = TRUE)

######################################################

# Dashboard construction
header <- dashboardHeader(title = "TrHackathon")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # info page
    menuItem(" About", tabName = "about", icon = icon("question")),
    
    menuItem(" Athletes", icon = icon("user"),
      # summary for each athlete
      menuSubItem(" Summary", tabName = "summary",
                  icon = icon("list")),
      # compare athletes
      menuSubItem(" Head to head", tabName = "h2h",
                  icon = icon("users")) 
    ),
    # time-series forecasting for athlete's upcoming performances
    menuItem(" Forecasting", tabName = "forecast",
                icon = icon("chart-line")),
    
    # show effects of weather variables on performance
    menuItem(" Weather", tabName = "weather", icon = icon("cloud")),
    
    # show differences between events
    menuItem(" Events", tabName = "events", icon = icon("circle"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about", 
      fluidRow(
        box(slickROutput("slides"), width=12)
      )
    ),
    
    tabItem(tabName = "summary",
      fluidRow(
        box(
          selectInput("a_ev", "Event",
            c("Discus Throw", "Shot Put",
              "Hammer Throw"),
            selected = "Discus Throw"),
          selectInput("a_gen", "Gender",
            c("Male", "Female"),
            selected = "Male")
        ),
        
        box(
          uiOutput("select_ath")
        )
      ),
      
      # value boxes, 
      fluidRow(
        valueBoxOutput("PB"),
        valueBoxOutput("SB"),
        valueBoxOutput("medals")
      ),
            
      # STL decomposition
      fluidRow(
        plotOutput("stl")
      )
    ),
    
    tabItem(tabName = "h2h",
      fluidRow(
        box(
          selectInput("h_ev", "Event",
            c("Discus Throw", "Shot Put",
              "Hammer Throw"),
            selected = "Discus Throw"),
          selectInput("h_gen", "Gender",
            c("Male", "Female"),
            selected = "Male")
        ),
              
        box(uiOutput("h_select_aths"))
      ),
            
      fluidRow(
        box(plotOutput("h_traject"), width = 8),
              
        box(
          dateRangeInput("h_t_dr", "Date Range",
            start = "2000-01-01", end = "2025-01-01",
            min = "1985-01-01", max = "2025-01-01"
          ),
          selectInput("h_metric", "Metric",
            c("Metres", "World Athletics Points"),
            selected = "Metres"),
          
          width = 4
        )
      ),
            
      fluidRow(
        box(plotOutput("h_weather"), width = 8),
              
        box(
          selectInput("h_w_var", "Variable",
            c("Temperature", "Apparent Temperature",
              "Humidity", "Wind",
              "Precipitation", "Elevation"),
            selected = "Temperature"),
                
          selectInput("h_w_metric", "Metric",
            c("Metres", "World Athletics Points"),
            selected = "Metres"),
                
          selectInput("h_w_method", "Method",
            c("Linear Model", "Loess", "Auto"),
            selected = "Linear Model"),
          
          selectInput("h_w_io", "Indoor/Outdoor",
            c("Indoor", "Outdoor", "Both"),
            selected = "Outdoor"),
          
          width = 4
        )
      ),
            
      fluidRow(
        box(plotOutput("h_season"), width = 8),
              
        box(
          selectInput("h_s_metric", "Metric",
            c("Metres", "World Athletics Points"),
            selected = "Metres"),
          
          width = 4
        )
      )        
    ),
    
    tabItem(tabName = "forecast",
      fluidRow(
        box(plotOutput("event_predict"), width = 8), 
        
        box(
          selectInput("p_model", "Model",
            c("ARIMA", "ARIMA with exogenous variables",
              "Neural network", "Neural network with exogenous variables"),
            selected = "ARIMA with exogenous variables"), width = 4
        )
      ),
            
      fluidRow(
        box(plotOutput("forecast"), width = 8),
              
        box(
          selectInput("f_ath", "Athlete",
                      MDT_aths, selected = "Kristjan Čeh"),
          
          selectInput("f_model", "Model",
                      c("ARIMA", "ARIMA with exogenous variables",
                        "Neural network", "Neural network with exogenous variables"),
                      selected = "ARIMA with exogenous variables"),
          
          selectInput("f_tf", "Forecast Horizon",
                      c("3 Months", "9 Months"), selected = "9 Months"),
          
          width = 4
        )
      )
    ),
    
    tabItem(tabName = "weather",
      fluidRow(
        # weather effect plot
        box(plotOutput("w_eff"), width = 8),
        
        # drop-down menus for variables
        box(
          selectInput("w_var", "Variable",
              c("Temperature", "Apparent Temperature",
                "Humidity", "Wind",
                "Precipitation", "Elevation"),
              selected = "Temperature"),
          
          selectInput("w_event", "Event",
              c("Heavy Throws", "Shot Put", 
                "Discus Throw", "Hammer Throw"),
              selected = "Discus Throw"),
          
          selectInput("w_gender", "Gender",
              c("All", "Male", "Female"),
              selected = "All"),
          
          selectInput("w_metric", "Metric",
              c("Metres", "World Athletics Points"),
              selected = "World Athletics Points"),
          
          selectInput("w_disagg", "Disaggregation",
              c("None", "All-facet",
                "Gender-facet", "Gender-colour",
                "Event-facet", "Event-colour",
                "Gender-facet, Event-colour",
                "Event-facet, Gender-colour"),
              selected = "Gender-colour"),
          
          selectInput("w_io", "Indoor/Outdoor",
              c("Outdoor", "Indoor", 
                "Both-aggregated",
                "Both-disaggregated"),
              selected = "Outdoor"),
          
          selectInput("w_method", "Method",
              c("Linear Model", "Loess", "Auto"),
              selected = "Linear Model"),
          width = 4
        )
      )
    ),
    
    tabItem(tabName = "events", 
      fluidRow(
        # plot of average results by time in season
        box(plotOutput("ev_season"), width = 8),
        
        # drop-down menus for variables
        box(
          selectInput("ev_event", "Event",
            c("Heavy Throws", "Shot Put", 
              "Discus Throw", "Hammer Throw"),
            selected = "Heavy Throws"),
          
          selectInput("ev_gender", "Gender",
            c("All", "Male", "Female"),
            selected = "All"),
          
          selectInput("ev_metric", "Metric",
            c("Metres", "World Athletics Points"),
            selected = "World Athletics Points"),
          
          selectInput("ev_disagg", "Disaggregation",
            c("None", "All-facet",
              "Gender-facet", "Gender-colour",
              "Event-facet", "Event-colour",
              "Gender-facet, Event-colour",
              "Event-facet, Gender-colour"),
            selected = "Event-facet, Gender-colour"),
          
          width = 4
        )
      ),
            
      fluidRow(
        # plot of relationship between age and result
        box(plotOutput("age_eff"), width = 8),
        
        # drop-down menus for variables
        box(
          selectInput("ev_event2", "Event",
            c("Heavy Throws", "Shot Put", 
              "Discus Throw", "Hammer Throw"),
            selected = "Heavy Throws"),
          
          selectInput("ev_gender2", "Gender",
            c("All", "Male", "Female"),
            selected = "All"),
          
          selectInput("ev_metric2", "Metric",
            c("Metres", "World Athletics Points"),  
            selected = "World Athletics Points"),
          
          selectInput("ev_disagg2", "Disaggregation",
            c("None", "All-facet",
              "Gender-facet", "Gender-colour",
              "Event-facet", "Event-colour",
              "Gender-facet, Event-colour",
              "Event-facet, Gender-colour"),
            selected = "Event-facet, Gender-colour"),
          
          width = 4
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  # docs
  output$slides <- renderSlickR({
    imgs <- list.files("C:/Users/User/Documents/R/Trhackathon/www", 
                       pattern = ".jpg",
                       full.names = TRUE)
    slickR(imgs, width = "80%")
  })
  
  # reactive drop-downs
  output$select_ath <- renderUI({
    selectInput("ath", "Athlete",
      get_athletes(event = input$a_ev,
                  gender = input$a_gen)
    )
  })
  
  output$h_select_aths <- renderUI({
    aths <- get_athletes(event = input$h_ev,
                         gender = input$h_gen)
    selectInput("h_aths", "Athlete",
      aths, selected = c(aths[1],aths[2]),
      multiple = TRUE
    )
  })
  
  # value boxes
  output$PB <- renderValueBox({
    req(input$ath)
    
    valueBox(
      paste0(get_PB_SB(event = input$a_ev,
                       ath = input$ath,
                       SB = FALSE), "m"),
      "Personal Best",
      icon = icon("star"),
      color = "green"
    )
  })
  
  output$SB <- renderValueBox({
    req(input$ath)
    
    valueBox(
      paste0(get_PB_SB(event = input$a_ev,
                       ath = input$ath,
                       SB = TRUE), "m"),
      "Season's Best",
      icon = icon("arrow-up")
    )
  })
  
  output$medals <- renderValueBox({
    req(input$ath)
    
    n_medals <- get_medals(event = input$a_ev,
                           ath = input$ath)
    
    valueBox(
      n_medals,
      paste0("Major Medal", 
             ifelse(n_medals > 1, "s", "")),
      icon = icon("medal"),
      color = "yellow"
    )
  })
  
  # plots
  output$stl <- renderPlot({
    req(input$ath)
    
    plot_STL(event = input$a_ev,
             ath = input$ath)
  })
  
  output$event_predict <- renderPlot({
    mdl <- case_when(
      input$p_model == "ARIMA" ~ "arima",
      input$p_model == "ARIMA with exogenous variables" ~ "arimax",
      input$p_model == "Neural network" ~ "nnet",
      .default = "nnetx"
    )
    plot_EC_predict(model = mdl,
                    aths = MDT_aths, 
                    date = "2024-06-07")
  })
  
  output$forecast <- renderPlot({
    if (input$f_tf == "3 Months") {
      fc <- MDT_forecast_short
    } else {
      fc <- MDT_forecast
    }
    
    mdl <- case_when(
      input$f_model == "ARIMA" ~ "arima",
      input$f_model == "ARIMA with exogenous variables" ~ "arimax",
      input$f_model == "Neural network" ~ "nnet",
      .default = "nnetx"
    )
    
    plot_forecast(fc = fc,
                  model = mdl,
                  ath = input$f_ath)
  })
  
  output$h_traject <- renderPlot({
    req(input$h_aths)
    
    compare_trajectories(aths = input$h_aths,
                         start_date = input$h_t_dr[1],
                         end_date = input$h_t_dr[2],
                         event = input$h_ev,
                         metric = input$h_metric)
  })
  
  output$h_weather <- renderPlot({
    req(input$h_aths)
    
    weather_effect(variable = input$h_w_var,
                   event = input$h_ev,
                   metric = input$h_w_metric,
                   method = input$h_w_method,
                   disagg = "name",
                   in_out = input$h_w_io,
                   aths = input$h_aths)
  })
  
  output$h_season <- renderPlot({
    req(input$h_aths)
    
    seasonal_effect(event = input$h_ev,
                    metric = input$h_s_metric,
                    disagg = "name",
                    aths = input$h_aths)
  })
  
  output$ev_season <- renderPlot({
    seasonal_effect(event = input$ev_event,
                    gender = input$ev_gender,
                    metric = input$ev_metric,
                    disagg = input$ev_disagg)
  })
  
  output$age_eff <- renderPlot({
    age_trend(event = input$ev_event2,
              gender = input$ev_gender2,
              metric = input$ev_metric2,
              disagg = input$ev_disagg2)
  })
  
  output$w_eff <- renderPlot({
    weather_effect(variable = input$w_var,
                   event = input$w_event,
                   gender = input$w_gender,
                   metric = input$w_metric,
                   disagg = input$w_disagg,
                   in_out = input$w_io,
                   method = input$w_method)
  })
}

shinyApp(ui, server)