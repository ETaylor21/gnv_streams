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


#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams')

#call in data file

data=read_csv('gnv_streams_data_run_mtg20190222.csv')

#reshape data to organize

data_spread=spread(data = data, key = 'Parameter', value = 'Result')
as.data.frame(data_spread)

#create new csv with spread data

write_excel_csv(data_spread, path = 'C:/Users/etaylor21/Documents/gnv_streams/gnv_streams_data_run_mtg20190222.csv', 
                na = 'NA', append =FALSE, col_names = TRUE)


