##Data run for SFS conference on 5/19/2019

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

########################Call in and format data########################################

#call in packages

library(tidyverse)
library(lubridate)
library(RColorBrewer)

#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams/Stream Survey - Summer 2019')

#call in data file

data_nut = read_csv('TUM_nutrientdata_20190624.csv', col_types = cols(
  Site = col_character(),
  REP = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Analyte = col_character(),
  Result = col_double()))#nutrient data; fixed date format for date column


data_fp = read_csv('TUM_fpdata_20190624.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Analyte = col_character(),
  Result = col_double()))#field parameter data


#change date to show only as month; will need to change as POR spans > than 1 year

#lubridate::floor_date(data_nut$Date, unit = 'month')
#lubridate::floor_date(data_fp$Date, unit = 'month')

#####Nutrient Time-Series (nts)#####

nd2 = data_nut %>% 
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))


windows()
nut_ts = ggplot(nd2, aes(x = Site, y = mean, group = 1))

nut_ts + geom_line() + ylab('Results (mg/L)') +
  geom_point(size = 4, aes(x=Site, color = Site)) +
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
              labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                       NOx = "Nitrate-Nitrite (N)", 
                                       OP = 'Orthophosphate (P)'))) + 
  theme(strip.text = element_text(size = 15)) + 
  scale_x_discrete(limits = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))


#####Field Parameter Time Series####

windows()

fp2 = data_fp %>% 
  filter(Analyte == 'DO_Sat' | Analyte == 'pH' | Analyte == 'SpCond' | Analyte == 'Temp' | Analyte == 'Turb')

#fp2

field_param_ts = ggplot(fp2, aes(x = Site, y = Result, group = 1))

field_param_ts + geom_line() + ylab(NULL) + 
  geom_point(size = 4, aes(x=Site, color = Site)) + 
    facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(DO_Sat = "DO Saturation (%)", 
                                       pH = 'pH (SU)', 
                                       SpCond = 'SpCond (uS/cm)', 
                                       Temp = 'Temperature (degC)', 
                                       Turb = 'Turbidity (NTU)' ))) + 
  theme(strip.placement = 'outside', strip.text = element_text(size = 11), 
        axis.text = element_text(size = rel(1.1)), 
        axis.title.x = element_text(),
        legend.position = 'none') +
  scale_x_discrete(limits = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))


