# Human Resources: Full-time instructional staff by academic rank, faculty and tenure status,
# race/ethnicity and gender
# 2012-2019
# Select all academic ranks within faculty status

library(tidyverse)
library(stringr)

setwd("C:/Users/Colton/Desktop/WOUApp")

faculty = read.csv("./Faculty/Faculty_FullTimeXAcademicRank.csv")

faculty_clean = faculty %>%
  pivot_longer(!c(UnitID, Institution.Name)) %>%
  mutate(year = str_extract(name, "[0-9]{4}")) %>%
  mutate(race = str_extract(name, "(?:(?!.total).)*")) %>%
  mutate(rank = str_extract(name, "(?<=staff..).*")) %>%
  mutate(across(c(race, rank), ~gsub("\\.", " ", .))) %>%
  mutate(across(c(race, rank), ~gsub("s $", "", .))) %>%
  mutate(across(c(rank), ~toTitleCase(.))) %>%
  group_by(year, rank) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(!race %in% c("Grand")) %>%
  mutate(percent = value/total) %>%
  mutate(race = ifelse(race == "Hispanic or Latino", "Hispanic", race))

write.csv(faculty_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2012-2019 Faculty.csv")

         