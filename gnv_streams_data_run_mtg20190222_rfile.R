##Data run for Lab Meeting on 2/22/2019

##Parameter List with Units
#DO - dissolved oxygen (mg/L)
#DO_Sat - dissolved oxygen saturation (%)
#Temp - water temperature (deg C)
#SpCond - specific conductivity (uS/cm)
#pH - pH (SU)
#Turb - turbidity (NTU)

####NOx, O_P, NH4 all filtered samples###

#NOx - Nitrate(NO3)-Nitrite(NO2)-N (mg/L)
#OP - orthophosphate(P) (mg/L)
#NH4 - ammonia(N) (mg/L)

####Sample Replications (col = Rep) 
#a = 1; b = 2; c = 3; d = not a rep just a reading

################################################################

#call in packages

library(tidyverse)
library(lubridate)

#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams')

#call in data file

data_nut=read_csv('gnv_streams_data_run_mtg20190222.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Time = col_time(format = ""),
  REP = col_character(),
  NH4 = col_double(),
  NOx = col_double(),
  OP = col_double())); problems(data_nut)#nutrient data; fixed date format for date column

data_nut$Date#double checked date converted correctly
  
data_fp=read_csv('gnv_streams_fieldparam_data_run_mtg20190222.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Time = col_time(format = ""),
  DO = col_double(),
  DO_Sat = col_double(),
  pH = col_double(),
  SpCond = col_double(),
  Temp = col_double(),
  Turb = col_double())); problems(data_fp)#field parameter data

#change date to show only as month; will need to change as POR spans > than 1 year

lubridate::floor_date(data_nut$Date, unit = 'month')
lubridate::floor_date(data_fp$Date, unit = 'month')

#reshaping data so all units read in a column

data_nut_2=gather(data_nut, 'NH4', 'NOx', 'OP', key = 'Analyte', value = "Result")

write_excel_csv(data_nut_2, path = 'C:/Users/etaylor21/Documents/gnv_streams/gnv_streams_data_run_mtg20190222.csv', 
                na = 'NA', append =FALSE, col_names = TRUE)

data_nut_2


data_fp_2=gather(data_fp, 'DO', 'DO_Sat', 'pH', 'SpCond', 'Temp', 'Turb', key = 'Analyte', value = "Result")

write_excel_csv(data_fp_2, path = 'C:/Users/etaylor21/Documents/gnv_streams/gnv_streams_data_run_mtg20190222_v2.csv', 
                na = 'NA', append =FALSE, col_names = TRUE)

data_fp_2

#read the nutrient REPs as averages instead of 3 individual points on a sampling trip

group_data_nut = group_by(data_nut_2, Site, Date, Analyte)#by which columns am I grouping my data by
group_data_nut#check the groups are correct
data_nut_USE_ME = summarise(group_data_nut, mean = mean(Result))#calculate average of result for analyte test



f = ggplot(data_nut_USE_ME, aes(x=Date, y=mean, group = Site, color = Analyte, shape = Site)) 

f + geom_point()

