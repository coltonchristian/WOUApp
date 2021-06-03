#2011-2019
# Adjusted Cohort
#All Students Total

library(tidyverse)
library(stringr)

setwd("C:/Users/Colton/Desktop/WOUApp")

graduations = read.csv("./Graduations/Graduations.csv")

graduations_clean = graduations %>%
  pivot_longer(!c(UnitID, Institution.Name)) %>%
  mutate(year = str_extract(name, "[0-9]{4}")) %>%
  mutate(race = str_extract(name, "(?:(?!.total).)*")) %>%
  mutate(across(c(race), ~gsub("\\.", " ", .))) %>%
  group_by(year) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(!race %in% c("Grand")) %>%
  mutate(percent = value/total) 

write.csv(graduations_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2011-2019 Graduations.csv")

         