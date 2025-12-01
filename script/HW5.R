---
title: "HW5"
author: "Amy Gill"
date: "2025-11-30"
output: word_document
--

packages <- c("tidyverse", "sf", "tigris", "forcats", "lubridate", "knitr")
install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(sf)
library(tigris)
library(forcats)
library(lubridate)

options(tigris_use_cache = TRUE)



homicides <- read_csv("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv")


#Choosing city
city_name <- "Baltimore"
state_abbrev <- "MD"

city_data <- homicides %>%
  filter(city == city_name, state == state_abbrev)

#Convert homicide points to sf 
city_sf <- city_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)


#Download Census boundaries with tigris and use tracts.
tracts <- tracts(
  state = state_abbrev,
  county = "510",
  year = 2020) %>%
  st_transform(4326)


#5

city_sf <- city_sf %>%
  mutate(
    race_clean = fct_lump(f = as.factor(victim_race), n = 3))


#6
city_sf <- city_sf %>%
  mutate(
    solved = case_when(
      disposition %in% c("Closed without arrest", "Open/No arrest") ~ "Unsolved",
      TRUE ~ "Solved"))


#7 Plot ~Homicides with county
ggplot() +
  geom_sf(data = tracts, fill = "gray95", color = "gray70") +
  geom_sf(data = city_sf, aes(color = race_clean), alpha = 0.8, size = 1.8) +
  facet_wrap(~solved) +
  scale_color_brewer(palette = "Dark2", name = "Race") +
  labs(
    title = paste("Homicides in", city_name, state_abbrev),
    subtitle = "Faceted by case status (Solved vs Unsolved)",
    caption = "Source: Washington Post homicide dataset") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 14, face = "bold"))


