#### Loading libraries ####
library(tidyverse)
library(lubridate)
library(data.table)
library(cowplot)


#### Loading Regions from UK to do inner join of data: ####
uk_nations <- read_csv("uk_nations.csv")

uk_regions <- read_csv("uk_regions.csv")

uk_geo <- uk_regions %>% left_join(uk_nations, by = "la")


#### Apple ####
##### Importing #####
apple <- read_csv("applemobilitytrends-2021-02-27.csv")

glimpse(apple)

##### Pipe transformation #####
apple %>% 
  filter(region %in% c("England", "Wales")) %>%
  pivot_longer(cols = starts_with("202"), names_to = "day", values_to = "mobility") %>% 
  select(country, region, transportation_type, day, mobility) %>% 
  mutate(# Transform day variable to date instead of character
         day = ymd(day),
         # Take week number for plot
         week = epiweek(day)) -> apple_v2



#### Google ####
##### Importing #####
google <- read_csv("Global_Mobility_Report.csv")

glimpse(google)

##### Pipe transformation #####
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
  mutate(week = epiweek(date)) %>% 
  mutate(mobility = case_when(type == "residential" ~ mobility * (-1),
                              TRUE ~ mobility)) -> google_v2
  
  
glimpse(google_v2)



#### Facebook ####
##### Importing #####
facebook <- fread("movement-range-2021-02-27.txt")

glimpse(facebook)

##### Pipe transformation #####
facebook %>% 
  filter(polygon_name %in% c("England", "Wales")) %>% 
  mutate(date = ymd(ds)) %>% 
  select(date, polygon_name, 
         all_day_bing_tiles_visited_relative_change,
         all_day_ratio_single_tile_users) %>% 
  mutate(week = epiweek(date)) ->  facebook_v2

glimpse(facebook_v2)


#### Exercise 1: Plotting ####
# Let's plot to see what we have in each data.frame

##### Apple #####
apple_v2 %>% 
  filter(year(day) == 2020, week <= 33) %>% 
  group_by(region, week, transportation_type) %>% 
  summarise(mean_mob = mean(mobility, na.rm = T)) %>% 
  ggplot(aes(x = week, y = mean_mob, color = transportation_type)) + geom_point() +
  facet_grid(~region)


##### Google #####
google_v2 %>% 
  filter(year(date) == 2020, week <= 33,
         nation %in% c("england", "wales")) %>% 
  group_by(nation, week, type) %>% 
  summarise(mean_mob = mean(mobility_fix, na.rm = T)) %>% 
  ggplot(aes(x = week, y = mean_mob, color = type)) + geom_point() +
  facet_grid(~nation)


##### Facebook #####
facebook_v2 %>% 
  filter(year(date) == 2020, week <= 33,
         polygon_name %in% c("England", "Wales")) %>%
  group_by(polygon_name, week) %>% 
  summarise(mean_mob = mean(all_day_bing_tiles_visited_relative_change, na.rm = T)) %>% 
  ggplot(aes(x = week, y = mean_mob)) + geom_point() +
  facet_grid(~polygon_name)



##### Putting everything together (same structure) #####

###### Google (base) ######
google_v3 <- google_v2 %>% 
                  filter(year(date) == 2020, week <= 33,
                          nation %in% c("england", "wales")) %>% 
                  select(nation, week, type, mobility) %>% 
                  mutate(source = "Google") %>% 
                  group_by(source, nation, type, week) %>% 
                  summarise(mean_mobility = mean(mobility, na.rm = TRUE))
              

###### Apple ######
apple_v3 <- apple_v2 %>%
              filter(year(day) == 2020, week <= 33) %>% 
              select(region, week, transportation_type, mobility) %>% 
              rename(nation = region,
                     type = transportation_type) %>% 
              mutate(source = "Apple") %>% 
              group_by(source, nation, type, week) %>% 
              summarise(mean_mobility = mean(mobility, na.rm = TRUE)) %>% 
              mutate(mean_mobility = mean_mobility - 100)

###### Facebook ######
facebook_v3 <- facebook_v2 %>% 
                filter(year(date) == 2020, week <= 33,
                        polygon_name %in% c("England", "Wales")) %>%
                select(polygon_name, week, all_day_bing_tiles_visited_relative_change) %>% 
                rename(nation = polygon_name,
                       mobility = all_day_bing_tiles_visited_relative_change) %>% 
                mutate(source = "Facebook") %>% 
                group_by(source, nation, week) %>% 
                summarise(mean_mobility = mean(mobility, na.rm = TRUE)) %>% 
                mutate(mean_mobility = mean_mobility * 100,
                       type = "mobility")


###### All together now! ######

ggplot(data = google_v3, aes(x = week, y = mean_mobility, color = interaction(source, type))) + geom_line(linetype = "dotdash", size = 1) +
  geom_point(data = apple_v3, aes(x = week, y = mean_mobility, color = interaction(source, type))) +
  geom_line(data = facebook_v3, aes(x = week, y = mean_mobility, color=interaction(source, type)), size = 1) +
  facet_wrap(~ toupper(nation)) +
  labs(color = "Source : Category") +
  #theme(legend.title = element_text()) +
  theme_bw()
  



#### Exercise 2: Missing values in Google ####

# Only Google data
# All data, also the missing stuff

##### Pipe Transformation #####
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
  ### Using uk_regions for joining this time, since we don't care for Nations variable
  left_join(uk_regions, by = "la") %>% 
  ###
  select(country_region, la, region, date,
         retail, grocery, parks, transit, workplaces, residential) %>% 
  pivot_longer(cols = c("retail", "grocery", "parks", "transit", "workplaces", "residential"), 
               names_to = "type", 
               values_to = "mobility") %>% 
  mutate(week = epiweek(date)) %>% 
  filter(year(date) == 2020, week <= 33) %>% 
  mutate(mobility = case_when(type == "residential" ~ mobility * (-1),
                              TRUE ~ mobility),
         miss_flag = case_when(is.na(mobility) == TRUE ~ 1,
                               TRUE ~ 0)) %>%
  group_by(region, type, week) %>% 
  summarise(share_miss = sum(miss_flag),
            obs = n()) -> google_miss

##### Plotting missing data by region and week #####
google_miss %>% 
  filter(region %in% c("east of england", "east midlands", "london",
                       "north east england", "north west england", "south east england",
                       "south west england", "wales", "west midlands", "yorkshire and the humber")) %>% 
  ggplot(aes(x = week, y = share_miss/obs, color = type)) + geom_point() +
  facet_wrap(~region, nrow = 3)



