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



# Loading data from Google:
google <- read_csv("Global_Mobility_Report.csv")

glimpse(google)

google %>% 
  filter(country_region == "United Kingdom") %>%
  rename(la = sub_region_1,
         retail = retail_and_recreation_percent_change_from_baseline,
         grocery = grocery_and_pharmacy_percent_change_from_baseline,
         parks = parks_percent_change_from_baseline,
         transit = transit_stations_percent_change_from_baseline,
         workplaces = workplaces_percent_change_from_baseline,
         residential = residential_percent_change_from_baseline
         ) %>%
  mutate(la = tolower(la)) %>% 
  left_join(uk_nations, by = "la") %>% 
  select(country_region, la, nation, date,
         retail, grocery, parks, transit, workplaces, residential) %>% 
  filter(!is.na(nation)) -> google_v2
  
  



