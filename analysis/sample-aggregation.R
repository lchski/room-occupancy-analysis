sample_sensor_ids <- sensor_day_unoccupied_starts %>%
  distinct(sensor_id) %>%
  slice(1:5) %>%
  pull

room_occupancy_minutes <- room_occupancy_events %>%
  filter(sensor_id %in% sample_sensor_ids) %>%
  mutate(
    minute = map2(timestamp, next_timestamp, seq, by = "1 min") ## take each timestamp/next timestamp pair and create a list of the minutes between them (inclusive)
  ) %>%
  unnest(minute) ## expand that list so each minute becomes its own row

room_occupancy_minutes %>%
  select(-timestamp, -next_timestamp) %>%
  mutate(hour_minute = strftime(minute, "%H:%M", tz = "UTC")) %>%
  mutate(is_occupied = occupied_status == "Occupied") %>%
  group_by(hour_minute, is_occupied) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  mutate(prop_clean = if_else(
    ! is_occupied & prop == 1,
    0,
    prop
  )) %>%
  filter(
    is_occupied |
    prop_clean == 0
  ) %>%
  mutate(
    count_clean = if_else(
      is_occupied,
      count,
      as.integer(0)
    )
  ) %>%
  select(hour_minute, is_occupied_percentage = prop_clean, is_occupied_count = count_clean) %>%
  write_csv("data/out/room_occupancy_minute_proportions.csv")
