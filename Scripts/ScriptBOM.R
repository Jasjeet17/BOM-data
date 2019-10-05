library(tidyverse)
read_csv("data/BOM_data.csv") # read in data file
BOM_data <- read_csv("data/BOM_data.csv")# assign a variable
# Question1- solution1
BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("Tempmin", "Tempmax"), sep = "/") #separate temperatures min and max
BOM_data_separated #print the dataset
BOM_data_filtered <- BOM_data_separated %>% #filter out the non-numeric values
  filter(Tempmin != '-') %>%
  filter(Tempmax != '-') %>%
  filter(Rainfall != '-') %>%
  group_by(Station_number) %>% #group by station numbers
  summarise (n = n()) #count the number of rows
BOM_data_filtered

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
gather (station_number, value, -info)
BOM_stations

BOM_stations <- read_csv("data/BOM_stations.csv") #read data set1
BOM_data <- read_csv("data/BOM_data.csv") #read data set 2
stations_very_long <- BOM_stations %>%
gather (station_number, values, -info)
stations_tidy <- stations_very_long %>%
  spread(info, values)
stations_tidy
stations_tidy_numeric <- mutate (stations_tidy, station_number = as.numeric(station_number))
stations_tidy_numeric
BOM_stations_all <- full_join(BOM_data_separated,stations_tidy_numeric, by = c("Station_number" = "station_number")) #but the station number in the two data files is different (chr vs dbl) 
q3_ans <- BOM_stations_all %>% 
mutate(t_diff = as.numeric(tempmax) - as.numeric(tempmin)) %>%
filter(!is.na(t_diff)) %>% 
group_by(state) %>% 
summarise(avg_t_diff = mean(t_diff)) %>% 
arrange(avg_t_diff)
q3_ans
