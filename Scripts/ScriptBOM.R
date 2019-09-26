library(tidyverse)
read_csv("data/BOM_data.csv")
BOM_data <- read_csv("data/BOM_data.csv")
# Question1- solution1
BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("Tempmin", "Tempmax"), sep = "/")
BOM_data_separated
BOM_data_separated %>%
  filter(Tempmin != '-') %>%
  filter(Tempmax != '-') %>%
  filter(Rainfall != '0') %>%
  group_by(Station_number) %>%
  summarise (n = n())
#Question 1- solution2
BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("Tempmin", "Tempmax"), sep = "/")
BOM_data_separated
BOM_data_separated %>%
  filter(Tempmin == min(Tempmin)) %>%
  filter(Tempmax == max(Tempmax)) %>%
  filter(Rainfall == max(Rainfall)) %>%
  group_by(Station_number) %>%
  summarise (n = n())
