# Loading libraries
library(tidyverse)
library(lubridate)


# Loading Regions from UK to do inner join of data:
uk_nations <- read_csv("uk_nations.csv")

uk_regions <- read_csv("uk_regions.csv")

uk_geo <- uk_regions %>% left_join(uk_nations, by = "la")

rm(uk_nations)
rm(uk_regions)



# Loading data from Apple:
apple <- read_csv("applemobilitytrends-2021-02-27.csv")

glimpse(apple)

apple %>% 
  filter(region %in% c("England", "Wales")) %>%
  pivot_longer(cols = starts_with("202"), names_to = "day", values_to = "mobility") %>% 
  select(country, region, transportation_type, day, mobility) %>% 
  mutate(# Transform day variable to date instead of character
         day = ymd(day),
         # Take week number for plot
         week = epiweek(day)) -> apple_v2










