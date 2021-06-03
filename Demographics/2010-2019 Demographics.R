library(tidyverse)
library(janitor)

# Download ACS data for race from link below:
# https://data.census.gov/cedsci/table?q=race%2Fethnicity%20oregon&tid=ACSDT1Y2018.B02001
# https://data.census.gov/cedsci/table?q=race&tid=ACSDP1Y2019.DP05

 
setwd("C:/Users/Colton/Desktop/WOUApp")

demo_csvs = data.frame(files = list.files("./Demographics/productDownload_2021-06-01T143832", full.names =  TRUE)) %>%
  mutate(data = grepl("data_with_overlays", files)) %>%
  filter(data == TRUE)

demo = map_dfr(unique(demo_csvs$files), ~read.csv(.x, skip=1) %>% mutate_all(as.character) %>%
                          mutate(year = str_extract(.x, "([0-9]{4}(?=\\.{1}))"))) %>%
  select(year, ends_with("any.race."), contains("NOT.HISPANIC"), -contains("Margin"), -contains("Percent")) %>%
  clean_names(.) %>%
  select(-ends_with("hispanic_or_latino")) %>%
  pivot_longer(!year, names_to = "race", values_to = "value") %>%
  mutate(race_extract = ifelse(grepl("hispanic_or_latino_of_any_race", race), "Hispanic",
                        ifelse(grepl("white", race), "White",
                        ifelse(grepl("black", race), "Black or African American",
                        ifelse(grepl("american_indian", race),  "American Indian or Alaska Native", 
                        ifelse(grepl("native_hawaiian", race),  "Native Hawaiian or Other Pacific Islander", 
                        ifelse(grepl("two_or", race), "Two or More Races", 
                        ifelse(grepl("asian", race), "Asian",
                        ifelse(grepl("some", race), "Race ethnicity unknown", 
                                      race))))))))) %>%
  filter(!grepl("two_races", race)) %>%
  group_by(year, race_extract) %>%
  summarize(value = sum(as.numeric(value), na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(total = sum(value)) %>%
  ungroup() %>%
  mutate(percent = value/total) %>%
  select(year, race = race_extract, value, total, percent) 

write.csv(demo, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2010-2019 Demographics.csv")


##### Not Used


# demo_data = data.frame(File = demo_csvs$files) %>%
#  extract(File, "year", "([0-9]{4}(?=\\.{1}))", remove = FALSE) %>%
#  map_dfr(~)
#  mutate(Data = lapply(File, read_csv, skip = 1)) %>%
#  unnest(Data) %>% 
#  clean_names(.) %>%
#  as.data.frame(.) %>%
#  select(year, starts_with("estimate"))

# demo_data_2019 = demo_data %>%
#   filter(year == 2019) %>%
#   select(year, contains("_2")) %>%
#   rename_all(~(stringr::str_replace_all(., '_2', '')))
# 
# demo_data_all = demo_data %>%
#   filter(year != 2019) %>%
#   select(year, !contains("_2")) %>%
#   bind_rows(., demo_data_2019) %>%
#   pivot_longer(contains("estimate")) %>%
#   rename(race = "name") %>%
#   mutate(race = recode(race, 
#                        "estimate_total" = "Grand",
#                        "estimate_total_white_alone" = "White",                                                                   
#                        "estimate_total_black_or_african_american_alone" = "Black or African American",                                             
#                        "estimate_total_american_indian_and_alaska_native_alone" = "American Indian or Alaska Native",                                      
#                        "estimate_total_asian_alone" = "Asian",                                                                  
#                        "estimate_total_native_hawaiian_and_other_pacific_islander_alone" = "Native Hawaiian or Other Pacific Islander",                           
#                        "estimate_total_some_other_race_alone" = "Race ethnicity unknown",                                                        
#                        "estimate_total_two_or_more_races" =  "Two or more races")) %>%
#   filter(!race %in% c("estimate_total_two_or_more_races_two_races_including_some_other_race",
#                       "estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races")) %>%
#   group_by(year) %>%
#   mutate(percent = value/value[race == "Grand"])
