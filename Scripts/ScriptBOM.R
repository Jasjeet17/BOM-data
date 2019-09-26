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
Tidy_BOM_stations <- BOM_stations %>% #assign a variable
gather (station_ID, value, -info) %>% #gather column headings to single column
spread (info, value) %>% 
mutate (station_ID = as.numeric(station_ID))
Tidy_BOM_stations
write_csv(Tidy_BOM_stations, "results/Tidy_BOM_stations.csv")
new_BOM_data <- full_join(Tidy_BOM_stations, BOM_data)



