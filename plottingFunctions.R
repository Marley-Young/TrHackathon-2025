#########################################################
# Utility and plotting functions for use in dashboard.R #
#########################################################

require(tidyverse)
require(DescTools)
require(imputeTS)
require(fpp3)
require(cowplot)

# load data 
load("df_EC_heavy_throws.Rda")
ts_MDT <- readRDS("ts_MDT.rds")

# load pre-computed forecasts
MDT_forecast <- readRDS("MDT_forecast.rds")
MDT_forecast_short <- readRDS("MDT_forecast_short.rds")
MDT_forecast_full <- readRDS("MDT_forecast_full.rds")
stl_comps <- readRDS("stl_comps.rds")

# Helper functions:
get_event <- function(event) case_when(
  event == "Shot Put" ~ "SP",
  event == "Discus Throw" ~ "DT",
  event == "Hammer Throw" ~ "HT",
  .default = "all"
)

get_event_long <- function(event) case_when(
  event == "SP" ~ "Shot Put",
  event == "DT" ~ "Discus Throw",
  .default = "Hammer Throw"
)

get_gender <- function(gender) case_when(
  gender == "Male" ~ "M",
  gender == "Female" ~ "W",
  .default = "all"
)

get_gender_long <- function(gender) ifelse(gender == "M",
                                           "Male", "Female")

get_PB_SB <- function(dat = df_EC_heavy_throws,
                   event, ath, SB = FALSE) {
  ev <- get_event(event)
  d <- dat |> filter(event == ev, 
                     name == ath)
  if (SB) d <- d |> filter(year(date) == 2024)
  return(max(d$result))
}

get_medals <- function(dat = df_EC_heavy_throws,
                       event, ath) {
  ev <- get_event(event)
  d <- dat |> filter(event == ev, 
                     name == ath,
                     comp_class == "Major",
                     rank != "",
                     as.numeric(rank) <= 3)
  return(nrow(d))
}

get_athletes <- function(dat = df_EC_heavy_throws, 
                         event, gender,
                         EC_final = FALSE) {
  ev <- get_event(event)
  gen <- get_gender(gender)
  d <- dat |> 
    filter(event == ev,
           gender == gen)
  
  if (EC_final) d <- d |>
    filter(comp_name == "26th European Athletics Championships")
  
  d <- d |> select(name) |>
    distinct()
  
  return(d$name)
}

get_colour <- function(disagg) case_when(
  disagg %in% c("Event-colour", "Gender-facet, Event-colour") ~ "event",
  disagg %in% c("Gender-colour", "Event-facet, Gender-colour") ~ "gender",
  disagg == "name" ~ "name",
  .default = "None"
)

get_facet <- function(disagg, io_disagg = FALSE) {
  if(io_disagg) {
    if (disagg == "All-facet") {
      fcet <- facet_wrap(~gender+event+indoor_outdoor,
                         labeller = labeller(
                           gender = get_gender_long,
                           event = get_event_long
                         ))
    } else if (disagg %in% c("Gender-facet, Event-colour", 
                             "Gender-facet")) {
      fcet <- facet_wrap(~gender+indoor_outdoor,
                         labeller = labeller(
                           gender = get_gender_long
                         ))
    } else if (disagg %in% c("Event-facet, Gender-colour",
                             "Event-facet")) {
      fcet <- facet_wrap(~event+indoor_outdoor,
                         labeller = labeller(
                           event = get_event_long
                         ))
    } else {
      fcet <- facet_wrap(~indoor_outdoor)
    }
  } else {
    if (disagg == "All-facet") {
      fcet <- facet_wrap(~gender+event,
                         labeller = labeller(
                           gender = get_gender_long,
                           event = get_event_long
                         ))
    } else if (disagg %in% c("Gender-facet, Event-colour", 
                             "Gender-facet")) {
      fcet <- facet_wrap(~gender,
                         labeller = labeller(
                           gender = get_gender_long
                         ))
    } else if (disagg %in% c("Event-facet, Gender-colour",
                             "Event-facet")) {
      fcet <- facet_wrap(~event,
                         labeller = labeller(
                           event = get_event_long
                         ))
    } else {
      fcet <- NULL
    }
  }
}

get_model_desc <- function(model) case_when(
  model == "arima" ~ "an ARIMA model",
  model == "arimax" ~ "an ARIMA model with exogenous regressors",
  model == "nnet" ~ "a neural network time series model",
  .default = "a neural network time series model with exogenous regressors"
)

# Plotting functions:

plot_STL <- function(comps = stl_comps,
                     event, ath) {
  ev <- get_event(event)
  
  d <- comps |>
    filter(event == ev,
           name == ath) |>
    autoplot()
  
  sttl <- paste("We can view the athlete's performance",
               "over time as being largely determined",
               "by an overall trend and seasonal variations")
  
  p <- d$data |>
    ggplot(aes(x=yw, y=.val)) +
    geom_line(colour = "red") +
    facet_wrap(~.var,
               labeller = labeller(
                 .var = c(
                   result = "Result (m)",
                   trend = "Trend Component",
                   season_year = "Seasonal Component",
                   remainder = "Remainder")
               ),
               ncol = 1,
               scales = "free_y"
    ) +
    scale_x_yearweek(date_breaks = "1 year", 
                     date_labels = "%Y") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white")
    ) +
    labs(title = paste0(ath, ": STL Decomposition"), 
         subtitle = sttl,
         x = "Date", y = "Components")
  
  return(p)
}

# compare athletes' performance trends
compare_trajectories <- function(dat = df_EC_heavy_throws,
                                 event = "All",
                                 gender = "All",
                                 aths,
                                 start_date = "2000-01-01",
                                 end_date = "2025-01-01",
                                 metric = "World Athletics Points",
                                 show_legend = TRUE,
                                 lev = 0.8, se = TRUE) {
  ev <- get_event(event)
  gen <- get_gender(gender)
  met <- ifelse(metric == "Metres", "result", "iaaf_points")
  
  d <- dat |> filter(
    name %in% aths,
    date >= start_date,
    date <= end_date
  )
  if (ev != "all") d <- d |> filter(event == ev)
  if (gen != "all") d <- d |> filter(gender == gen)
  
  p <- d |> ggplot(aes(x=date, y=.data[[met]], colour = name)) +
    geom_smooth(aes(fill = name), alpha = 0.2, level = lev,
                se = se, show.legend = show_legend)
  
  # note performances at major games in particular
  p + geom_point(data = (d |> filter(comp_class == "Major")), 
                 aes(x=date, y=.data[[met]], colour = name), 
                 size=3, show.legend = show_legend) +
    labs(title = "Career Trajectory", 
         caption = "The dots represent results at major championships",
         colour = "Name",
         fill = "Name") +
    xlab("Date") +
    ylab(if(metric == "Metres") "Result (m)" else "World Athletics Points") +
    theme_cowplot()
}

# Describe the variation of average heavy
# throws performance throughout year
seasonal_effect <- function(dat = df_EC_heavy_throws,
                            event = "Heavy Throws",
                            gender = "All",
                            metric = "World Athletics Points",
                            disagg = "Event-facet, Gender-colour",
                            aths = "all") {
  ev <- get_event(event)
  gen <- get_gender(gender)
  met <- ifelse(metric == "Metres", "result", "iaaf_points")
  clr <- get_colour(disagg)
  fcet <- get_facet(disagg, FALSE)
  
  gen2 <- case_when(
    gen == "M" ~ "mens' ",
    gen == "W" ~ "womens' ",
    .default = ""
  )
  
  ttl <- paste0("Variation of ", gen2, tolower(event),
                " performance throughout the year")
  
  if (event %in% c("Discus Throw", "Hammer Throw") |
      disagg %in% c("None", "Gender-colour", "Gender-facet")) {
    capt <- ""
  } else {
    capt <- paste0("Notice the small peak early in the year in \n",
                   "the case of the shot put. This is presumably \n",
                   "due largely to the indoor season.")
  }
  
  yl <- ifelse(metric == "Metres", "Result (m)", "World Athletics Points")
  
  d <- dat
  if (aths[1] != "all") d <- d |> filter(name %in% aths)
  if (ev != "all") d <- d |> filter(event == ev)
  if (gen != "all") d <- d |> filter(gender == gen)
  
  if (clr != "None") {
    p <- d |>
      ggplot(aes(x=as.Date(
        paste0("2018-", format(date, "%m-%d"))), 
        y=.data[[met]], colour=.data[[clr]])) +
      geom_smooth()
  } else {
    p <- d |>
      ggplot(aes(x=as.Date(
        paste0("2018-", format(date, "%m-%d"))), 
        y=.data[[met]])) +
      geom_smooth()
  }
  
  if (!is.null(fcet)) p <- p + fcet
  
  return(p + labs(title = ttl, caption = capt,
                  colour = StrCap(clr, "first"),
                  x = "Date", y = yl) +
           scale_x_date(date_breaks = "4 months",
                        date_labels = "%b") +
           theme_cowplot())
}

# Describe the relationship between heavy throws
# performance and age
age_trend <- function(dat = df_EC_heavy_throws,
                      event = "Heavy Throws",
                      gender = "All",
                      metric = "World Athletics Points",
                      disagg = "Event-facet, Gender-colour") {
  ev <- get_event(event)
  gen <- get_gender(gender)
  met <- ifelse(metric == "Metres", "result", "iaaf_points")
  clr <- get_colour(disagg)
  fcet <- get_facet(disagg, FALSE)
  
  gen2 <- case_when(
    gen == "M" ~ "mens' ",
    gen == "W" ~ "womens' ",
    .default = ""
  )
  
  ttl <- paste0("Trend of ", gen2, tolower(event),
                " performance with age")
  
  if (disagg %in% c("None", "Gender-colour", "Gender-facet")) {
    capt <- ""
  } else {
    capt <- paste0("Notice for example that hammer throwers \n",
                   "tend to start to decline in performance \n",
                   "sooner than shot putters")
  }
  
  yl <- ifelse(metric == "Metres", "Result (m)", "World Athletics Points")
  
  d <- dat
  if (ev != "all") d <- d |> filter(event == ev)
  if (gen != "all") d <- d |> filter(gender == gen)
  
  if (clr != "None") {
    p <- d |>
      ggplot(aes(x=age_weeks/52, y=.data[[met]], colour=.data[[clr]])) +
      geom_smooth()
  } else {
    p <- d |>
      ggplot(aes(x=age_weeks/52, y=.data[[met]])) +
      geom_smooth()
  }
  
  if (!is.null(fcet)) p <- p + fcet
  
  return(p + labs(title=ttl, caption = capt,
                  x = "Age (years)", y = yl,
                  colour = StrCap(clr, "first")) +
           theme_cowplot())
}

# Describe the effect of a given weather variable
# (or elevation) on average performance (by event/gender
# if desired) in the heavy throws
weather_effect <- function(dat = df_EC_heavy_throws,
                           variable = "Temperature",
                           event = "Heavy Throws",
                           gender = "All",
                           metric = "World Athletics Points",
                           disagg = "None",
                           in_out = "Outdoor",
                           method = "Linear Model",
                           aths = "all") {
  var <- case_when(
    variable == "Apparent Temperature" ~ "avg_apparent_temperature",
    variable == "Humidity" ~ "avg_relative_humidity_2m",
    variable == "Wind" ~ "avg_wind_speed_10m",
    variable == "Precipitation" ~ "avg_precipitation",
    variable == "Elevation" ~ "elevation",
    .default = "avg_temperature_2m"
  )
  
  ev <- get_event(event)
  gen <- get_gender(gender)
  met <- ifelse(metric == "Metres", "result", "iaaf_points")
  io_disagg <- if_else(in_out == "Both-disaggregated", TRUE, FALSE)
  clr <- get_colour(disagg)
  
  if (method == "Loess") {
    mth <- "loess"
  } else if (method == "Auto") {
    mth <- NULL
  } else {
    mth <- "lm"
  }
  
  fcet <- get_facet(disagg, io_disagg)
  
  gen2 <- case_when(
    gen == "M" ~ "mens' ",
    gen == "W" ~ "womens' ",
    .default = ""
  )
  
  io <- case_when(
    in_out == "Outdoor" ~ "outdoor ",
    in_out == "Indoor" ~ "indoor ",
    .default = ""
  )
  
  ttl <- paste0("Effect of ", tolower(variable), " on ",
               gen2, io, tolower(event), " performance")
  
  xl <- case_when(
    variable == "Apparent Temperature" ~ "Apparent Temperature (°C)",
    variable == "Humidity" ~ "Relative Humidity (%)",
    variable == "Wind" ~ "Average Windspeed (km/h)",
    variable == "Precipitation" ~ "Average Precipitation (mm)",
    variable == "Elevation" ~ "Elevation (m)",
    .default = "Temperature (°C)"
  )
  
  yl <- ifelse(metric == "Metres", "Result (m)", "World Athletics Points")
  
  # Add caption(s)
  
  d <- dat
  if (aths[1] != "all") d <- d |> filter(name %in% aths)
  if (ev != "all") d <- d |> filter(event == ev)
  if (gen != "all") d <- d |> filter(gender == gen)
  if (in_out %in% c("Outdoor", "Indoor")) d <- d |> 
    filter(indoor_outdoor == in_out)
  
  if (clr != "None") {
    p <- d |> 
      ggplot(aes(x = .data[[var]], y=.data[[met]], color = .data[[clr]])) +
      geom_smooth(method = mth)
  } else{
    p <- d |> 
      ggplot(aes(x = .data[[var]], y=.data[[met]])) +
      geom_smooth(method = mth)
  }
  
  if (!is.null(fcet)) p <- p + fcet
  
  return(p + labs(title = ttl, colour = StrCap(clr, "first")) +
           xlab(xl) + ylab(yl) + theme_cowplot())
}

plot_forecast <- function(ts = ts_MDT,
                          fc = MDT_forecast,
                          model = "arimax",
                          ath) {
  f <- fc |> 
    filter(name == ath,
           .model == model)
  
  ttl <- paste0("Forecast of ", ath, "'s ",
                "discus throw performance")
  sttl <- paste0("Using ", get_model_desc(model),
                ", trained on prior data")
  xl <- "Date"
  yl <- "Result (m)"
  capt <- paste0("The black line displays actual results. \n",
                 "Notice how the inclusion of exogenous variables \n",
                 "tends to improve the local shape of the forecast \n",
                 "regardless of how accurate the trend is.")
  
  p <- f |> autoplot(ts) +
    scale_x_yearweek(date_breaks = "1 year",
                     date_labels = "%Y") +
    labs(title = ttl, subtitle = sttl,
         caption = capt, x = xl, y = yl) +
    theme_cowplot()
  
  return(p)
}

plot_EC_predict <- function(fc = MDT_forecast_full,
                            model = "arimax", 
                            aths, date) {
  f <- fc |> 
    filter(name %in% aths,
           .model == model,
           yw == yearweek(date))
  
  ttl <- "Forecast results for the men's discus at the 2024 European Championships"
  sttl <- paste0("Using ",  get_model_desc(model),
                ", trained on prior data")
  capt <- "The red dots indicate actual results"
  xl <- "Result (m)"
  yl <- "Athlete"
  
  p <- f |>
    ggplot(aes(x = .mean, y = name, 
               xmin = hilo(result, 89)$lower,
               xmax = hilo(result, 89)$upper)) +
    geom_pointrange(colour = "blue") +
    geom_pointrange(data = df_EC_heavy_throws |>
      filter(name %in% aths,
             comp_name == "26th European Athletics Championships"), 
      aes(x=result, y=name, xmin=result, xmax=result),
      colour = "red") +
    labs(title = ttl, subtitle = sttl, caption = capt) +
    xlab(xl) + ylab(yl) + theme_cowplot()
  
  return(p)
}
