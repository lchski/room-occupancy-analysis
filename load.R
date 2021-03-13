library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

room_occupancies_raw <- read_excel("data/source/ODFP_Meeting-Rooms-Sample.xlsx")

room_occupancies <- room_occupancies_raw %>%
  pivot_longer(cols = -`Floor ID`:-`Space Class`, names_to = "minute", values_to = "is_occupied") %>%
  clean_names %>%
  mutate(date = as_date(date)) %>%
  mutate(minute = as.POSIXct(as.numeric(minute) * 1440 * 60, origin = "2021-01-01", tz = "UTC")) %>%
  mutate(minute_fmt = strftime(minute, "%H:%M", tz = "UTC"))

sensor_day_unoccupied_starts <- room_occupancies_raw %>%
  clean_names %>%
  distinct(sensor_id, date = as_date(date)) %>%
  mutate(timestamp = as_datetime(date)) %>%
  mutate(occupied_status = "Unoccupied")

room_occupancies_raw %>%
  clean_names %>%
  select(floor_id:space_class, -day) %>% ## get rid of everything but the core rows we need
  select(-floor_id, -room_name, -space_class) %>% ## TODO: remove this line later; just for easier debugging
  rename(
    timestamp = x24h_time,
    occupied_status = y_enum
  ) %>%
  mutate(
    date = as_date(date),
    timestamp = round_date(ymd_hms(paste0(date, timestamp), tz = "UTC"), "1 minute"),
  ) %>%
  bind_rows(sensor_day_unoccupied_starts) %>%
  arrange(sensor_id, timestamp) %>%
  mutate(row_number = row_number()) %>%
  group_by(sensor_id, date) %>%
  mutate(
    next_timestamp = lead(timestamp)
  ) %>%
  mutate(
    next_timestamp = if_else(
      timestamp == next_timestamp,
      next_timestamp,
      next_timestamp - minutes(1)
    )
  ) %>%
  mutate(
    next_timestamp = if_else(
      is.na(next_timestamp),
      ceiling_date(timestamp, "1 day") - minutes(1),
      next_timestamp
    )
  ) %>%
  group_by(row_number) %>%
  mutate(
    minute = map2(timestamp, next_timestamp, seq, by = "1 min")
  ) %>%
  unnest(minute)
  

