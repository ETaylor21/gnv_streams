#install.packages( 
# c("LakeMetabolizer","unitted","dplyr","lazyeval","lubridate","magrittr",
#   "tidyr","chron","dygraphs","ggplot2","RCurl","rstan","XML","xts"),
#  repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
#devtools::install_github("USGS-R/streamMetabolizer")
#devtools::find_rtools()

library(streamMetabolizer)
library(tidyverse)
library (devtools)
library(StreamPULSE)
library(dplyr)
library(lubridate)

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


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.2006 ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

HAT_metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

as.data.frame(HAT_metab_inputs)


bayes_name <- mm_name(type='bayes', pool_K600='binned' , err_obs_iid=TRUE, err_proc_iid=TRUE)# I specified 'binned' in poll_k600 after recieving the error that discharge data should only be included if & only if pool_k600_type indicates hierarchy...not sure what that means but setting the poll_k600 to normal did nothing so I tried binned and it ran
bayes_name

#bayes_specs <- specs(bayes_name, burnin_steps = 50, saved_steps = 100)
#bayes_specs

mm <- metab(specs(bayes_name, burnin_steps = 50, saved_steps = 100), data = HAT_metab_inputs)

mm

output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'HAT_Test_1_Output.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)
