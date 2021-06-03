#Excludes Law, Medicine, Dentistry
#All Students Total

library(tidyverse)
library(stringr)

setwd("C:/Users/Colton/Desktop/WOUApp")

majors = read.csv("./Major/Data_6-2-2021.csv")

majors_clean = majors %>%
  pivot_longer(!c(UnitID, Institution.Name)) %>%
  mutate(year = str_extract(name, "[0-9]{4}")) %>%
  mutate(race = str_extract(name, "(?:(?!.total).)*")) %>%
  mutate(major = str_extract(name, "(?<=[0]{4}\\.)(.*)(?=\\.\\.)")) %>%
  mutate(across(c(race, major), ~gsub("\\.", " ", .))) %>%
  filter(major != "Engineering") %>%
  group_by(year, major) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(race != "Grand") %>%
  mutate(percent = value/total)

write.csv(majors_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2010-2018 Majors.csv")
  
  
  

         