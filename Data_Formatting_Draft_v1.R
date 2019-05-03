#install.packages('tidyverse')
#install.packages('stringi')
#install.packages('plyr')
library(tidyverse)


p = read_csv('C:/Users/Emily/Documents/SN_20472339_2019-04-03_14_43_35_-0400.csv', col_names = TRUE)


p %>%

  as_tibble() %>% 
  rename(DateTime = 'Date Time, GMT-05:00') %>%
  rename(Temp = 'Temp, °C') %>%
  rename(lumft2 = 'Intensity, lum/ft²')



