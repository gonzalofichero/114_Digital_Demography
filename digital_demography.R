# Loading libraries
library(tidyverse)
library(lubridate)
library(data.table)


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
  filter(!is.na(nation)) %>% 
  pivot_longer(cols = c("retail", "grocery", "parks", "transit", "workplaces", "residential"), 
               names_to = "type", 
               values_to = "mobility") %>% 
  mutate(week = epiweek(date)) -> google_v2
  
  
glimpse(google_v2)



# Loading data from Facebook:
facebook <- fread("movement-range-2021-02-27.txt")

glimpse(facebook)


facebook %>% 
  filter(polygon_name %in% c("England", "Wales")) %>% 
  mutate(date = ymd(ds)) %>% 
  select(date, polygon_name, 
         all_day_bing_tiles_visited_relative_change,
         all_day_ratio_single_tile_users) ->  facebook_v2

glimpse(facebook_v2)


# Let's plot to see what we have in each data.frame

# Apple:
apple_v2 %>% 
  filter(year(day) == 2020, week <= 33) %>% 
  group_by(region, week, transportation_type) %>% 
  summarise(mean_mob = mean(mobility, na.rm = T)) %>% 
  ggplot(aes(x = week, y = mean_mob, color = transportation_type)) + geom_point() +
  facet_grid(~region)

# Google:
google_v2 %>% 
  filter(year(day) == 2020, week <= 33) %>% 
  

