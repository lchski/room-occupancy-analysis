library(slider)

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

zzz %>%
  ungroup %>%
  filter(is_occupied) %>%
  mutate(prop_ma = slide_dbl(prop, mean, .before = 7, .after = 7)) %>%
  pivot_longer(c(prop, prop_ma)) %>%
  ggplot(aes(hour_minute, value, color = name)) +
  geom_point() +
  facet_grid(vars(name)) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour") +
  theme(axis.text.x = element_text(angle = 90))
