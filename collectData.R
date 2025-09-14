#############################################################################
# Script to parse TrHackathon json data into a data frame and join external #
# variables (weather, elevation).                                           #
#############################################################################

library(tidyverse)
library(jsonlite)
library(tidyjson)
library(openmeteo)
library(pkgcond)
library(countrycode)
library(elevatr)
library(comprehenr)

# Assume we have the TrHackathon dataset in a folder named "data"
athleteFileNames <- list.files(path = "data/athletes", pattern = "*.json", 
                               full.names = TRUE)

# Read all athletes' past competition data into a dataframe using jsonlite
flag <- FALSE
for (file in athleteFileNames) {
  dat <- read_json(file) %>% 
    as.tbl_json %>%
    spread_values(given_name = jstring("givenName"),
                  family_name = jstring("familyName"),
                  gender = jstring("gender"),
                  nationality = jstring("nationality"),
                  birthdate = jstring("birthDate"),
                  birthyear = jstring("birthYear"),
                  birthplace = jstring("birthPlace"),
                  height = jstring("height"),
                  weight = jstring("weight"),
                  ot_athlete_id = jstring("otAthleteId"),
                  tp_athlete_id = jstring("tpAthleteId")) %>%
    enter_object("results") %>%
    gather_array %>%
    spread_values(indoor_outdoor = jstring("IndoorOutdoor"),
                  event = jstring("event"),
                  ok_for_sb = jlogical("okForSB"),
                  result = jstring("result"),
                  iaaf_points = jstring("iaafpoints"),
                  wind = jstring("wind"),
                  rank = jstring("rank"),
                  round = jstring("round"),
                  round_stage = jstring("roundstage"),
                  age_group = jstring("agegroup"),
                  comp_id = jstring("competition_ID"),
                  comp_name = jstring("compName"),
                  date = jstring("date"),
                  city = jstring("city"),
                  venue_country = jstring("venuecountry"))
  
  if (flag == FALSE) {
    flag <- TRUE
    df <- dat
  } else {
    df <- bind_rows(df,dat)
  }
}
df$document.id <- NULL
df$array.index <- NULL
rm(dat)

# Add full name column, remove relays, convert date from string 
# to date format, add age variables, and add comp_class column 
# specifying significance of competition
df <- as_tibble(df) |> 
  filter(!is.na(given_name)) |> 
  mutate(name = paste(given_name, family_name, sep = " "),
         date = ymd(date),
         comp_class = case_when(
           str_detect(comp_name, "World Championships") ~ "Major",
           str_detect(comp_name, "World Athletics Championships") ~ "Major",
           str_detect(comp_name, "World Athletics Indoor Championships") ~ "Major",
           str_detect(comp_name, "Olympic Games") &
             !str_detect(comp_name, "Youth") ~ "Major",
           str_detect(comp_name, "European Athletics Championships") ~ "Major",
           str_detect(comp_name, "European Athletics Indoor Championships") ~ "Major",
           str_detect(comp_name, "Diamond League") ~ "Diamond League",
           .default = "Other"),
         iaaf_points = as.numeric(iaaf_points),
         age_weeks = floor(as.numeric(difftime(date, birthdate, 
                                               units = "weeks"))) ) |> 
  filter(age_weeks > 600) # remove some anomalous entries
         

# Get list of competitors by event competing at the
# 2024 European Champs (to filter out performances of athletes
# in events they don't specialise in)
get_all_EC <- function(){
  ec_df <- as_tibble(df) |> filter(
    comp_name == "26th European Athletics Championships",
    rank != "D",
    rank != "H"
  ) |> distinct(name, event)
  return(paste(ec_df$name, ec_df$event))
}

# restrict to valid finals performances
df_EC <- df |> filter(
  paste(name, event) %in% get_all_EC(),
  round_stage == "Final",
  !is.na(iaaf_points)
) |> distinct()

df_EC_by_events <- function(events, gend) {
  if (gend == "A") {
    return(df_EC |> filter(event %in% events))
  } else {
    return(
      df_EC |> filter(
        event %in% events,
        gender == gend) )
  }
}

# restrict to heavy throws for now
df_EC_heavy_throws <- df_EC_by_events(c("SP", "DT", "HT"), "A")

################################################################################

# Geocoding given city name and country;
# manually handles some unusual city name formats in the data,
# and makes approximations/guesses for remaining oddities
# (not totally accurate)
custom_geocode <- function(cty, ctry) {
  if (cty == "Ramona OK") {
    return(c(36.4664, -95.9956)) 
  }
  loc <- case_when(
    cty == "Roma" && ctry == "ITA" ~ "Rome",
    cty == "Bruxelles" ~ "Brussels",
    cty == "Alexándria" ~ "Alexándreia",
    cty == "Alfaz del Pi" ~ "L'Alfàs del Pi",
    cty == "Alta do Lumiar" ~ "Lisbon",
    cty == "Andrezieux Boutheon" ~ "Andrézieux-Bouthéon",
    cty == "Artemónas Sífnou" ~ "Artemonas",
    cty == "Banjaluka" ~ "Banja Luka",
    cty == "Bruay La Buissière" ~ "Bruay-La-Buissière",
    cty == "Cannes-la-Bocca" ~ "Cannes La Bocca",
    cty == "Castelnovo Monti" ~ "Castelnovo ne' Monti",
    cty == "Coetzenburg" ~ "Stellenbosch",
    cty == "Den Haag" ~ "The Hague",
    cty == "Filderstadt-Bernhausen" ~ "Bernhausen",
    cty == "Frankfurt-Kalbach" ~ "Kalbach",
    cty == "Haniá" ~ "Chania",
    cty == "Kamianets Podilskyi" ~ "Kamianets-Podilskyi",
    cty == "Jamor" ~ "Oeiras",
    cty == "Karlsbad" ~ "Langensteinbach",
    cty == "L'Etang Sale" ~ "L'Étang-Salé",
    cty == "Lemessos" ~ "Limassol",
    cty == "Lisboa" ~ "Lisbon",
    cty == "Montereau Fault Yonne" ~ "Montereau-Fault-Yonne",
    cty == "Mössingen-Belsen" ~ "Belsen",
    cty %in% c("Kirovohrad", "Kropyvnytskiy") ~ "Elizabethgrad",
    cty == "Naimette-Xhovémont" ~ "Liège",
    cty == "Nittedal" ~ "Rotnes",
    cty == "Oud Beijerland" ~ "Oud-Beijerland",
    cty == "Overhalla" ~ "Ranemsletta",
    cty == "Peraaru" ~ "Roosilla",
    cty == "Pireás" ~ "Piraeus",
    cty == "Raasepori" ~ "Raseborg",
    cty == "Schielleiten" ~ "Schellerten",
    cty == "St. Wendel" ~ "Sankt Wendel",
    cty == "Villeneuve d'Ascq" ~ "Villeneuve-d'Ascq",
    str_detect(cty, "/") ~ str_replace(cty, "/.*", ""),
    str_detect(cty, "\\s[:upper:][:upper:]") ~ str_replace(cty,
                                      "\\s[:upper:][:upper:]", ""),
    .default = cty
  )
  ctry_code <- countrycode(ctry, origin='ioc', 
                           destination='iso2c')
  gc <- tryCatch({
    tst <- geocode(loc, 50) |>
      filter(country_code == ctry_code)
    if(nrow(tst) == 0) {
      tst <- geocode(loc)
    }
    return(c(tst$latitude[1], tst$longitude[1])) }, 
    error = function(msg) {
      return(geocode(countrycode(ctry, origin = 'ioc',
                                destination = 'country.name')))
    }
  )
  return(c(gc$latitude[1], gc$longitude[1]))
}

# helper function for mutating with vector valued functions
to_tibble <- function (x, colnames) {
  x |>
    matrix(ncol = length(colnames), 
           dimnames = list(NULL, colnames)) |>
    as_tibble()
}

# Approximate venue locations
get_venues <- function(dat) dat |>
  group_by(city, venue_country) |>
  summarise() |>
  rowwise() |>
  mutate(
    to_tibble(custom_geocode(city, venue_country), 
              c("latitude", "longitude")) )

# join locations
df_EC_heavy_throws <- left_join(
  df_EC_heavy_throws,
  get_venues(df_EC_heavy_throws),
  by = c("city", "venue_country") 
)

# function to retrieve elevation (in metres)
get_elevation <- function(dat) {
  dat_locs <- dat |>
    group_by(longitude, latitude, city, venue_country) |>
    summarise()
  
  dat_elev_empty <- data.frame(
    x = dat_locs$longitude,
    y = dat_locs$latitude,
    city = dat_locs$city,
    venue_country = dat_locs$venue_country)
  
  dat_elev_data <- get_elev_point(locations = dat_elev_empty,
                                  prj = 4326, src = "aws")
  dat_elev_data$geometry <- NULL
  dat_elev_data$elev_units <- NULL
  
  return(left_join(dat, dat_elev_data,
                   by = c("city", "venue_country")))
}

df_EC_heavy_throws <- get_elevation(df_EC_heavy_throws)

df_EC_heavy_throws <- df_EC_heavy_throws |>
  mutate(
    result = as.numeric(result),
    elevation = ifelse(elevation < 0, 0, elevation) )

# Split throws by event
df_EC_DT <- df_EC_heavy_throws |> filter(event == "DT")
df_EC_HT <- df_EC_heavy_throws |> filter(event == "HT")
df_EC_SP <- df_EC_heavy_throws |> filter(event == "SP")

################################################################################

# calculate average of a given weather variable over
# feasible competition times
avg_hourly_weather <- function(lat, long, date, variables) {
  if(date > ymd("1940-01-01")) {
    # pause to avoid minutely API request limit
    Sys.sleep(0.5)
    weather_df <- weather_history(location = c(lat, long),
                                  start=date, end=date,
                                  hourly = variables) |>
      filter(
        hour(datetime) >= 11,
        hour(datetime) <= 19 )
    
    return(to_vec(for(v in variables)
        mean(weather_df[[paste0("hourly_", v)]])))
  } else {
    return(NULL)
  }
}

# function to summarise weather data in tbl
get_weather <- function(dat, variables) dat |>
  group_by(latitude, longitude, date) |>
  summarise() |>
  rowwise() |>
  mutate(
    to_tibble(
      avg_hourly_weather(latitude, longitude,
                   date, variables),
      to_vec(for(v in variables) paste0("avg_", v)))
  )

# join weather data (this can't be done all at once
# as it would hit API limits)
df_EC_DT <- left_join(
  df_EC_DT,
  get_weather(df_EC_DT, c(
    "apparent_temperature", "relative_humidity_2m",
    "wind_speed_10m", "precipitation", "pressure_msl",
    "temperature_2m")),
  by = c("latitude", "longitude", "date")
)

df_EC_HT <- left_join(
  df_EC_HT,
  get_weather(df_EC_HT, c(
    "apparent_temperature", "relative_humidity_2m",
    "wind_speed_10m", "precipitation", "pressure_msl",
    "temperature_2m")),
  by = c("latitude", "longitude", "date")
)

df_EC_SP <- left_join(
  df_EC_SP,
  get_weather(df_EC_SP, c(
    "apparent_temperature", "relative_humidity_2m",
    "wind_speed_10m", "precipitation", "pressure_msl",
    "temperature_2m")),
  by = c("latitude", "longitude", "date")
)

# rejoin throws data
df_EC_heavy_throws <- bind_rows(df_EC_DT, df_EC_HT, df_EC_SP)

# save as .Rda files
save(df_EC_heavy_throws, file = "df_EC_heavy_throws.Rda")

# write to csv
write.csv(df_EC_heavy_throws, "heavyThrows.csv", row.names = FALSE)
