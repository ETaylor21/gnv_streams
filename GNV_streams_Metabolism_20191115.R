install.packages(
  c("LakeMetabolizer","unitted","dplyr","lazyeval","lubridate","magrittr",
    "tidyr","chron","dygraphs","ggplot2","RCurl","rstan","XML","xts"),
  repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
devtools::install_github("USGS-R/streamMetabolizer")
devtools::find_rtools()

library(streamMetabolizer)
library(tidyverse)
library (devtools)
library(StreamPULSE)
library(dplyr)


HAT = read_csv('C:/Users/Emily/Downloads/SPdata_2019-11-13/FL_HAT_sensorData.csv')

HATDischarge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HAT_2019-09-01_XX.csv')

HATPressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

HAT2 = spread(HAT, key = variable, value = value)

HAT2

HATPres = HATPressure %>% 
  filter(datetime >= as.Date('2019-03-25') & datetime <=  as.Date('2019-07-29'))

HATWaterTemp = HAT2 %>%
  filter(DateTime_UTC >= as.Date('2019-03-25') & DateTime_UTC <=  as.Date('2019-07-29'))
  
  
  
  
DO = bind_cols(HATPres, HATWaterTemp)

DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C)
DO

HATDOSat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson')

HATDOSat  

HATDepth = calc_depth(Q = DO$Discharge_m3s)

HATDepth

bind_cols(DO, HATDepth, HATDOSat)
