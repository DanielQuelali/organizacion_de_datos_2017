library(lubridate)
library(tidyverse)
library(forcats)
library(plotly)
library(stringr)
library(magrittr)
library(xgboost)
library(Matrix)

trip_events_02 <- readRDS("data/processed/trip_events_02.Rds")
station_02 <- readRDS("data/processed/station_02.Rds")
weather_02 <- readRDS("data/processed/weather_02.Rds")

trip_events_02 %>% View("trips")

station_02 %>% View("stations")

weather_02 %>% View("weather")

all_trips <- trip_events_02 %>%
  filter(
    event == 'start'
  ) %>% # 669959 rows
  mutate(
    local_zip_code = zip_code == station_zip_code,
    zip_code_lump = fct_lump(zip_code, prop = 0.01)
  ) %>%
  inner_join(
    weather_02,
    by = c("station_zip_code" = "zip_code", "date")
  ) %>%
  # "T" value discarded from precipitation variable
  mutate_at(
    "precipitation_inches",
    as.numeric
  ) %>%
  select(
    bike_id,
    subscription_type,
    zip_code_lump,
    local_zip_code,
    station_id,
    station_zip_code,
    bikes_running,
    month,
    day_of_week,
    is_weekend,
    hour,
    max_temperature_f,
    mean_temperature_f,
    min_temperature_f,
    max_dew_point_f,
    mean_dew_point_f,
    min_dew_point_f,
    max_humidity,
    mean_humidity,
    min_humidity,
    mean_sea_level_pressure_inches,
    mean_visibility_miles,
    max_wind_Speed_mph,
    mean_wind_speed_mph,
    max_gust_speed_mph,
    precipitation_inches,
    cloud_cover,
    wind_dir_degrees,
    rain,
    fog,
    thunderstorm,
    duration
  )

all_trips %>% View("train")

xgb_params <- list(
  objective = "reg:linear"
)

all_trips

all_trips %>% {
  df <- .
  
  df %>%
    model.frame(
      ~ .,
      .,
      na.action = na.pass
    ) %>%
    model.matrix(
      duration ~ . - 1,
      .
    ) %>%
    xgb.DMatrix(
      label = df$duration,
      missing = NaN
    )
} %>%
  xgb.cv(
    params = xgb_params,
    data = .,
    nrounds = 10000,
    nfold = 5,
    early.stop.round = 100,
    print.every.n = 50,
    maximize = FALSE
  )
