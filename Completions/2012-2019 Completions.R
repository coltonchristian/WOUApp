#2011-2012 through 2018-2019
#All Students Total

library(tidyverse)
library(stringr)

setwd("C:/Users/Colton/Desktop/WOUApp")

completions = read.csv("./Completions/Completions.csv")

completions_clean = completions %>%
  pivot_longer(!c(UnitID, Institution.Name)) %>%
  mutate(year = str_extract(name, "[0-9]{4}")) %>%
  mutate(race = str_extract(name, "(?:(?!.total).)*")) %>%
  mutate(across(c(race), ~gsub("\\.", " ", .))) %>%
  group_by(year) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(!race %in% c("Grand", "Nonresident alien")) %>%
  mutate(percent = value/total) %>%
  mutate(race = ifelse(race == "Hispanic or Latino", "Hispanic", race))

write.csv(completions_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2012-2019 Completions.csv")

         