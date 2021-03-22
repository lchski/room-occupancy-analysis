zzz <- room_occupancy_minutes %>%
  #filter(sensor_id %in% sensor_ids_to_process) %>%
  select(-timestamp, -next_timestamp) %>%
  mutate(hour_minute = strftime(minute, "%H:%M", tz = "UTC")) %>%
  mutate(is_occupied = occupied_status == "Occupied") %>%
  group_by(hour_minute, is_occupied) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  rename(hour_minute_fmt = hour_minute) %>%
  mutate(hour_minute = ymd_hm(paste0("1970-01-01T", hour_minute_fmt), tz = "UTC"))

zzz2 %>%
  ungroup %>%
  filter(is_occupied) %>%
  ggplot(aes(hour_minute, prop)) +
  geom_point()
