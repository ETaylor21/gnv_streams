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

setwd('C:/Users/Emily/Documents/gnv_streams/SFS Poster')

#call in data file

data_nut = read_csv('gnv_nutrientdata_20190502.csv', col_types = cols(
  Site = col_character(),
  REP = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Time = col_time(format = ""),
  Analyte = col_character(),
  Result = col_double()))#nutrient data; fixed date format for date column


data_fp = read_csv('gnv_fpdata_20190502.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Time = col_time(format = ""),
  Analyte = col_character(),
  Result = col_double()))#field parameter data


#change date to show only as month; will need to change as POR spans > than 1 year

#lubridate::floor_date(data_nut$Date, unit = 'month')
#lubridate::floor_date(data_fp$Date, unit = 'month')

#####Nutrient Faceted Boxplot (nbp)####

nd2 = data_nut %>% 
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))

nd2

npb = ggplot(nd2, aes(x = Site, y = mean, fill = Analyte)) +
  geom_boxplot() + ylab('Results (mg/L)')

npb2 = npb + facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
                         labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                                  NOx = "Nitrate-Nitrite (N)", 
                                                  OP = 'Orthophosphate (P)'))) +
  theme(strip.text = element_text(size = 18)) +
  scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73' )) + guides(fill = FALSE) +
  theme(axis.text = element_text(size = rel(1.2))) + 
  theme(axis.title = element_text(size = rel(1.3)))



npb2

#####Nutrient Time-Series (nts)#####

nut_ts = ggplot(nd2, aes(x = Date, y = mean, fill = Site))

nts = nut_ts + geom_line(aes(group = Site, color = Site)) + ylab('Results (mg/L)') + 
  geom_point(size = 2, aes(x=Date, color = Site, shape = Site)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
              labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                       NOx = "Nitrate-Nitrite (N)", 
                                       OP = 'Orthophosphate (P)'))) + 
  theme(strip.text = element_text(size = 15))

nts + scale_color_manual(values = c('#56B4E9','#D55E00', '#009E73', '#CC79A7', '#0072B2', '#E69F00' )) + 
  guides(fill = FALSE)

#####Nutrient Data of just Possum Creek and Hogtown Upstream (pchu_ts)#####

pchu = data_nut %>% 
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-01-25') ) %>%
  filter(Site == 'HOGNW16' | Site == 'POSNW16') %>%
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))

#only using data from before january because HOGNW16 has been closed for construction and the gap in data looks wierd

pchu

windows()
pchu_nut_ts = ggplot(pchu, aes(x = Date, y = mean, fill = Site))

pchu_ts = pchu_nut_ts + geom_line(aes(group = Site, color = Site)) + ylab('Results (mg/L)') + 
  geom_point(size = 4, aes(x=Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = 'month', date_labels = '%b') +
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
              labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                       NOx = "Nitrate-Nitrite (N)", 
                                       OP = 'Orthophosphate (P)'))) +
  theme(strip.text = element_text(size = 18)) + 
  scale_color_brewer(palette = 'Dark2') + guides(fill = FALSE) +
  theme(axis.text = element_text(size = rel(1.5))) + 
  theme(axis.title = element_blank()) +
  theme(legend.position = c(0.8, 0.95)) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank(), 
        legend.text = element_text(size = rel(1.2))) +
  theme(legend.title = element_blank())

pchu_ts

#####Field Parameter Time Series####

fp2 = data_fp %>% 
  filter(Analyte == 'DO_Sat' | Analyte == 'pH' | Analyte == 'SpCond' | Analyte == 'Temp' | Analyte == 'Turb') %>%
  group_by(Site, Analyte)

fp2

field_param_ts = ggplot(fp2, aes(x = Date, y = Result))

fpts = field_param_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x=Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = 'month', date_labels = '%b') +
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(DO_Sat = "DO Saturation (%)", 
                                       pH = 'pH (SU)', 
                                       SpCond = 'SpCond (uS/cm)', 
                                       Temp = 'Temperature (degC)', 
                                       Turb = 'Turbidity (NTU)' ))) + 
  theme(strip.placement = 'outside', strip.text = element_text(size = 11), 
        axis.text = element_text(size = rel(1.1)), 
        axis.title.x = element_blank(),
        legend.position = 'bottom')

fpts + scale_color_manual(values = c('#56B4E9','#D55E00', '#009E73', '#CC79A7', '#0072B2', '#E69F00' )) +
  guides(fill = FALSE, col = guide_legend(nrow = 1)) 
