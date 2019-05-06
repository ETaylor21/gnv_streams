##Data run for CLCE Poster Presentation; March 20, 2019

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
library(RColorBrewer)

#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams')

#call in field parameter data file

data_nut=read_csv('gnv_streams_data_run_mtg20190222.csv')

data_fp=read_csv('gnv_streams_data_run_mtg20190222_v2.csv')

data_fp = drop_na(data_fp)

data_nut
data_fp

############subsetting data to be not include September 2018 samplng date############

#Drop the data from Sept

nd = data_nut %>% 
      filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-02-06')) %>%
      group_by(Site, Analyte) %>%
      summarise(mean = mean(Result), std.error = sqrt(var(Result)/length(Result)))#calculate average of result for analyte test and standard error

nd

#####Nutrient Bar Chart plotting############

nutrient = ggplot(nd, aes(x = Site, y = mean, fill = Analyte))

p = nutrient + geom_col(position = 'dodge') + ylab('Results (mg/L)') #make column chart

pf = p + geom_errorbar(aes(ymin = mean-std.error, ymax = mean+std.error), width = 0.2, position = position_dodge(0.9)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
              labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                       NOx = "Nitrate-Nitrite (N)", 
                                       OP = 'Orthophosphate (P)'))) +
  theme(strip.text = element_text(size = 15))#adding error bars to column chart and creating facets; 
  #in facet wrap the first changes whether panels are arranged horizantally or vertically, second allows sites to be different on the y, 
  #third changes the amont of rows to 3 and columns to 0 so that figures stack and headers stay on top

pf + scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73' )) + guides(fill = FALSE)#changes colors on bar fill to colorblind friendly

###############Nutrient Box Plot (nbp)################
nd2 = data_nut %>% 
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-02-06')) %>%
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))

nd2

npb = ggplot(nd2, aes(x = Site, y = mean, fill = Analyte)) +
  geom_boxplot() + ylab('Results (mg/L)')

npb + scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73' )) 

#faceted boplots

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

nts + scale_color_manual(values = c('#56B4E9','#D55E00', '#009E73', '#CC79A7', '#0072B2', '#E69F00' )) + guides(fill = FALSE)

#####Nutrient Data of just Possum Creek and Hogtown Upstream (pchu_ts)#####

pchu = data_nut %>% 
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-02-06') ) %>%
  filter(Site == 'HOGNW16' | Site == 'POSNW16') %>%
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))


pchu

pchu_nut_ts = ggplot(pchu, aes(x = Date, y = mean, fill = Site))

pchu_ts = pchu_nut_ts + geom_line(aes(group = Site, color = Site)) + ylab('Results (mg/L)') + 
  geom_point(size = 4, aes(x=Date, color = Site, shape = Site)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
                            labeller = as_labeller(c(NH4 = "Ammonium (N)", 
                                       NOx = "Nitrate-Nitrite (N)", 
                                       OP = 'Orthophosphate (P)'))) +
  theme(strip.text = element_text(size = 18)) + 
  scale_color_brewer(palette = 'Dark2') + guides(fill = FALSE) +
  theme(axis.text = element_text(size = rel(1.2))) + 
  theme(axis.title = element_text(size = rel(1.3))) +
  theme(legend.position = c(0.75,1), legend.justification = c(1,1)) + 
  theme(legend.background = element_blank()) + 
  theme(legend.key = element_blank()) + 
  theme(legend.title = element_blank())

pchu_ts



############################################################################################################

##############Field Parameter Bar Charts############

fp = data_fp %>% 
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-02-06') ) %>%
              filter(Analyte == 'DO_Sat' | Analyte == 'pH' | Analyte == 'SpCond' | Analyte == 'Temp' | Analyte == 'Turb') %>%
              group_by(Site, Analyte) %>% 
              summarise(mean = mean(Result), std.error = sqrt(var(Result)/length(Result)))#calculate average of result for analyte test and standard error

fp

fp2 = data_fp %>% 
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-02-06') ) %>%
  filter(Analyte == 'DO_Sat' | Analyte == 'pH' | Analyte == 'SpCond' | Analyte == 'Temp' | Analyte == 'Turb') %>%
  group_by(Site, Analyte)

fp2

#####Field Parameter Bar Chart (fpb)############

field_param = ggplot(fp, aes(x = Site, y = mean, fill = Analyte))

field_param_bar = field_param + geom_col(position = 'dodge') + ylab(NULL) #make column chart

fpb = field_param_bar + geom_errorbar(aes(ymin = mean-std.error, ymax = mean+std.error), width = 0.2, position = position_dodge(0.9)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(DO_Sat = "DO Saturation (%)", 
                                       pH = 'pH (SU)', 
                                       SpCond = 'SpCond (uS/cm)', 
                                       Temp = 'Temperature (degC)', 
                                       Turb = 'Turbidity (NTU)' ))) + 
                theme(strip.placement = 'outside', strip.text = element_text(size = 11))#adding error bars to column chart and creating facets; 
#in facet wrap the first changes whether panels are arranged horizantally or vertically, second allows sites to be different on the y, 
#third changes the amont of rows to 3 and columns to 0 so that figures stack and headers stay on top

fpb + scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73', '#CC79A7', '#0072B2' )) + guides(fill = FALSE)#changes colors on bar fill to colorblind friendly

#####Field Parameter Time Series (fpts)#####

field_param_ts = ggplot(fp2, aes(x = Date, y = Result))

fpts = field_param_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x=Date, color = Site, shape = Site)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(DO_Sat = "DO Saturation (%)", 
                                       pH = 'pH (SU)', 
                                       SpCond = 'SpCond (uS/cm)', 
                                       Temp = 'Temperature (degC)', 
                                       Turb = 'Turbidity (NTU)' ))) + 
  theme(strip.placement = 'outside', strip.text = element_text(size = 11))+
  theme(axis.text = element_text(size = rel(1.2))) + 
  theme(axis.title = element_text(size = rel(1.3)), legend.position = 'bottom') + 
  theme(legend.text = element_text(size = rel(1.3)))

#adding error bars to column chart and creating facets; 
#in facet wrap the first changes whether panels are arranged horizantally or vertically, second allows sites to be different on the y, 
#third changes the amont of rows to 3 and columns to 0 so that figures stack and headers stay on top

fpts + scale_color_manual(values = c('#56B4E9','#D55E00', '#009E73', '#CC79A7', '#0072B2', '#E69F00' )) +
  guides(fill = FALSE, col = guide_legend(nrow = 1))                                                                                                                  
