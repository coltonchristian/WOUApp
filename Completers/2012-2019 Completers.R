#2011-2012 through 2018-2019
#All Students Total
#Number of students receiving awards/degrees, by race/ethnicity and gender

library(tidyverse)
library(stringr)

setwd("C:/Users/Colton/Desktop/WOUApp")

completers = read.csv("./Completers/Completers.csv")

completers_clean = completers %>%
  pivot_longer(!c(UnitID, Institution.Name)) %>%
  mutate(year = str_extract(name, "[0-9]{4}")) %>%
  mutate(race = str_extract(name, "(?:(?!.total).)*")) %>%
  mutate(across(c(race), ~gsub("\\.", " ", .))) %>%
  group_by(year) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(!race %in% c("Grand", "Nonresident alien")) %>%
  mutate(percent = value/total) %>%
  mutate(race = ifelse(race == "Hispanic or Latino", "Hispanic", race))

write.csv(completers_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2012-2019 Completers.csv")

         