library(tidyverse)
read_csv("data/BOM_data.csv") # read in data file
BOM_data <- read_csv("data/BOM_data.csv")# assign a variable
# Question1- solution1
BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("Tempmin", "Tempmax"), sep = "/") #separate temperatures min and max
BOM_data_separated #print the dataset
BOM_data_separated %>% #filter out the non-numeric values
  filter(Tempmin != '-') %>%
  filter(Tempmax != '-') %>%
  filter(Rainfall != '-') %>%
  group_by(Station_number) %>% #group by station numbers
  summarise (n = n()) #count the number of rows

#Question 2- solution 1
BOM_data_separated %>% # Start with BOM data seperated 
  filter(Tempmin != '-') %>% # Filter all non-numeric data forms
  filter(Tempmax != '-') %>%
  filter(Rainfall != '-') %>%
  group_by(Month) %>% # group by month
  mutate (TempDiff =  as.numeric(Tempmax) - as.numeric(Tempmin)) %>% #add another column of Tempdiff
  summarise (MeanTempDiff = mean(TempDiff)) %>% #summarise Temp difference by calculating mean 
  arrange(MeanTempDiff) #arrange to find the lowest mean

#Question 3
BOM_stations %>%
gather (station_ID, value, -info)
BOM_stations

BOM_stations <- read_csv("data/BOM_stations.csv") #read data set1
BOM_data <- read_csv("data/BOM_data.csv") #read data set 2
stations_very_long <- BOM_stations %>%
gather (station_number, values, -info)
stations_tidy <- stations_very_long %>%
  spread(info, values)
stations_tidy
stations_tidy_numeric <- mutate (stations_tidy, station_number = as.numeric (station_number))
stations_tidy_numeric
BOM_combined_2 <- left_join(BOM_data_separated, stations_tidy)

BOM_combined_2
