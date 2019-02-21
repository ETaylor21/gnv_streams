#Data run without SWB stn for lab meeting on 02/22/2019

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

#call in nutrient data file

data_nut=read_csv('gnv_streams_data_run_mtg20190222.csv')

#change date to show only as month; will need to change as POR spans > than 1 year

lubridate::floor_date(data_nut$Date, unit = 'month')

#remove SWB from the data set 

data_nut_noSWB = data_nut %>% filter(Site != 'SWB')
data_nut_noSWB

#read the nutrient REPs as averages instead of 3 individual points on a sampling trip

group_data_nut_noSWB = group_by(data_nut_noSWB, Site, Date, Analyte)#by which columns am I grouping my data by
group_data_nut_noSWB#check the groups are correct
data_nut__noSWB_USE_ME = summarise(group_data_nut_noSWB, mean = mean(Result))#calculate average of result for analyte test
data_nut__noSWB_USE_ME#check that the average result is a single value

#####Nutrient plotting without SWB station############

f = ggplot(data_nut__noSWB_USE_ME, aes(x=Date, y=mean)) 

lol = f + geom_line(aes(group = Site, color = Site)) + geom_point(size = 2, aes(xend=Date, color = Site, shape = Site), yend = 0) + 
  scale_color_brewer(palette = 'Dark2') +
  theme(panel.grid.major.y = element_blank()) + facet_grid(Analyte ~ ., scales = 'free_y')


lol + ylab('Result')
