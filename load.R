library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

room_occupancies_raw <- read_excel("data/source/ODFP_Meeting-Rooms.xlsm") %>%
# room_occupancies_raw <- read_excel("data/source/ODFP_Meeting-Rooms-Sample.xlsx")
  clean_names %>%
  select(floor_id:space_class, -day) %>% ## get rid of everything but the core rows we need
  select(-floor_id, -room_name, -space_class) %>% ## TODO: remove this line later; just for easier debugging
  rename(
    timestamp = x24h_time,
    occupied_status = y_enum
  ) %>%
  mutate(
    date = as_date(date)
  )

## Get a list of sensor days (one row per sensor per day)
## These are the dummy "Unoccupied (Filler Data)" items that start each day
sensor_day_unoccupied_starts <- room_occupancies_raw %>%
  distinct(sensor_id, date) %>%
  mutate(timestamp = as_datetime(date)) %>%
  mutate(occupied_status = "Unoccupied (Filler Data)")

room_occupancy_events <- room_occupancies_raw %>%
  mutate(
    timestamp = paste0(date, timestamp),
    timestamp = if_else(
      hour(timestamp) == 23 & minute(timestamp) == 59,
      floor_date(ymd_hms(timestamp, tz = "UTC"), "1 minute"), ## round DOWN (because it's an 23:59)
      round_date(ymd_hms(timestamp, tz = "UTC"), "1 minute") ## round timestamps to nearest minute
    )
  ) %>%
  bind_rows(sensor_day_unoccupied_starts) %>% ## add in the dummy "Unoccupied" rows to start each day
  arrange(sensor_id, timestamp) %>% ## sort by sensor and timestamp
  mutate(row_number = row_number()) %>%
  select(row_number, everything()) %>%
  group_by(sensor_id, date) %>%
  mutate(
    next_timestamp = lead(timestamp) ## pull in the timestamp of the next event
  ) %>%
  mutate(
    next_timestamp = if_else(
      timestamp == next_timestamp, ## IF the timestamp is the same as the next timestamp
      next_timestamp, ## THEN keep them the same (this means the minutes don't roll backwards when we do the seq)
      next_timestamp - minutes(1) ## OTHERWISE subtract a minute from the next timestamp (so we don't overlap time periods)
    )
  ) %>%
  mutate(
    next_timestamp = if_else(
      is.na(next_timestamp), ## IF next_timestamp became NA in the above process (because it was the last event of the day for that sensor)
      ceiling_date(timestamp, "1 day") - minutes(1), ## THEN set next_timestamp to the last minute of the day
      next_timestamp ## OTHERWISE we gucci
    )
  )
  

room_occupancy_minutes <- room_occupancy_events %>%
  mutate(
    minute = map2(timestamp, next_timestamp, seq, by = "1 min") ## take each timestamp/next timestamp pair and create a list of the minutes between them (inclusive)
  ) %>%
  unnest(minute) ## expand that list so each minute becomes its own row

room_occupancy_minutes %>%
  summarize(count = n())




low_event_sensor_days <- room_occupancies_raw %>%
  count_group(sensor_id, date) %>%
  filter(count < 5) %>%
  mutate(tmp_id = paste(sensor_id, date)) %>%
  ungroup() %>%
  distinct(tmp_id) %>%
  pull
