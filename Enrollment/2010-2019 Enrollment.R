
library(tidyverse)
library(rsconnect)
library(openxlsx)



enrollment_files = list.files("C:/Users/Colton/Desktop/WOUApp/Enrollment", pattern = "*\\.zip", full.names = TRUE)

files = lapply(enrollment_files, unzip, exdir = "C:/Users/Colton/Desktop/WOUApp/Enrollment/unzipped")
csvfiles = bind_rows(lapply(list.files("C:/Users/Colton/Desktop/WOUApp/Enrollment/unzipped", full.names = TRUE, pattern = "*csv"), read.csv))
#names(csvfiles) =  gsub("^.*?\\.", "", names(csvfiles))

enrollment_clean = csvfiles %>%
  select(-IDX_EF, -contains("Level"), -year) %>%
  pivot_longer(!c(unitid:institution.name), names_to = "race", values_to = "value") %>%
  mutate(year = str_extract(race, "[0-9]{4}")) %>%
  filter(!is.na(value)) %>%
  mutate(race = gsub("^.*?\\.", "", race)) %>%
  mutate(race = gsub("\\.total", "", race)) %>%
  mutate(race = gsub("\\.", " ", race)) %>%
  filter(!grepl("men", race)) %>%
  filter(year != 2009) %>%
  group_by(year) %>%
  mutate(total = value[race == "Grand"]) %>%
  filter(race != "Grand") %>%
  mutate(percent = value/total)
  
write.csv(enrollment_clean, "C:/Users/Colton/Desktop/WOUApp/WOUapp/2010-2019 Enrollment.csv")
