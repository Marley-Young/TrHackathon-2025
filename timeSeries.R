#########################################################################
# Script to build a time series of men's discus data and make forecasts #
# using ARIMA and neural network models with exogenous variables        #
#########################################################################

library(tidyverse)
library(fpp3)
library(imputeTS)
library(bruceR)

# load data
load("df_EC_heavy_throws.Rda")

# restrict to finalists
finalists <- (df_EC_heavy_throws |>
                filter(comp_name == "26th European Athletics Championships") |>
                distinct(name))$name

# build tsibble of men's discus results with exogenous variables
ts_MDT <- df_EC_heavy_throws |>
  filter(name %in% finalists,
         event == "DT",
         gender == "M",
         date < "2024-06-03" | venue_country == "ITA") |>
  mutate(yw = yearweek(date),
         temp = scaler(avg_temperature_2m),
         humidity = scaler(avg_relative_humidity_2m),
         wind = scaler(avg_wind_speed_10m * 
                         as.numeric(indoor_outdoor == "Outdoor")),
         elevation = scaler(elevation)) |>
  group_by(name, yw) |>
  summarise(
    result = max(result),
    age = max(age_weeks),
    temp = mean(temp),
    humidity = mean(humidity),
    wind = mean(wind),
    elevation = mean(elevation)
  ) |>
  as_tsibble(key = name, index = yw)

# separate training sets
get_train <- function(ts, end, min_age) ts |>
  filter(yw <= end, age >= min_age) |>
  fill_gaps() |>
  na_interpolation() |>
  as_tsibble(key = name, index = yw)

ts_MDT_train <- get_train(ts_MDT, yearweek("2023-09-01"), 950)
ts_MDT_short <- get_train(ts_MDT, yearweek("2024-03-01"), 950)
ts_MDT_full <- get_train(ts_MDT, yearweek("2024-06-01"), 950)

# interpolate data (done after separating
# training set to avoid data leakage)
ts_MDT <- ts_MDT |>
  fill_gaps() |>
  na_interpolation() |>
  as_tsibble(key = name, index = yw)

# save time series
saveRDS(ts_MDT, "ts_MDT.rds")

# fit models
f_fit <- function(ts) ts |>
  model(
    arima = ARIMA(result),
    nnet = NNETAR(result),
    arimax = ARIMA(result ~ temp + humidity + wind + elevation),
    nnetx = NNETAR(result ~ temp + humidity + wind + elevation)
  )

MDT_fit <- f_fit(ts_MDT_train) 
MDT_fit_short <- f_fit(ts_MDT_train_short)
DT_fit_full <- f_fit(ts_MDT_train_full) 

# get last entry in training series
get_end <- function(dat, nam) {
  d <- dat |>
    filter(name == nam)
  return(max(d$yw))
}

# get test sets
get_test <- function(ts, ts_train, start) ts |>
  filter(yw > start) |>
  rowwise() |>
  filter(yw > get_end(ts_train, name)) |>
  as_tsibble(key=name, index=yw)
  
ts_MDT_test <- get_test(ts_MDT, ts_MDT_train,
                        yearweek("2023-02-01"))
ts_MDT_test_short <- get_test(ts_MDT, ts_MDT_train_short,
                              yearweek("2023-04-01"))
ts_MDT_test_full <- get_test(ts_MDT, ts_MDT_train_full,
                             yearweek("2023-06-01"))

# compute forecasts
MDT_forecast <- MDT_fit |>
  forecast(new_data = ts_MDT_test)

MDT_forecast_short <- MDT_fit_short |>
  forecast(new_data = ts_MDT_test_short)

MDT_forecast_full <- MDT_fit_full |>
  forecast(new_data = ts_MDT_test_full)

# save forecasts
saveRDS(MDT_forecast, "MDT_forecast.rds")
saveRDS(MDT_forecast_short, "MDT_forecast_short.rds")
saveRDS(MDT_forecast_full, "MDT_forecast_full.rds")

# compute STL components for all heavy throws athletes
stl_comps <- df_EC_heavy_throws |>
  mutate(yw = yearweek(date)) |>
  group_by(name, event, yw) |>
  summarise(result = max(result)) |>
  as_tsibble(key = c(name,event), index = yw) |>
  fill_gaps() |>
  na_interpolation() |>
  as_tsibble(key = c(name,event), index = yw) |>
  model(
    STL(result ~ trend() + 
          season(window = "periodic"),
        robust = TRUE) ) |>
  components()

# save STL components
saveRDS(stl_comps, "stl_comps.rds")
