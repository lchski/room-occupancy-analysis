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

room_occupancies
