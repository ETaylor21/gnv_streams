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

#download packages

#install.packages('fmsb')
#install.packages('car')
#install.packages('vegan')
#install.packages('animation')

#call in packages

library(plyr)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(fmsb)
library(car)
library(vegan)
library(animation)


#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams/SFS Poster')

#call in data file

data_nut = read_csv('gnv_nutrientdata_20190502.csv', col_types = cols(
  Site = col_character(),
  REP = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Analyte = col_character(),
  Result = col_double()))#nutrient data; fixed date format for date column


data_fp = read_csv('gnv_fpdata_20190502.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Time = col_time(format = ""),
  Analyte = col_character(),
  Result = col_double()))#field parameter data

#lubridate::floor_date(data_fp$Date, unit = 'month')


colors = c("gray20","gray40", "gray60", "gray80", rgb(19,149,186,maxColorValue=255),rgb(17,120,153,maxColorValue=255),rgb(17,120,153,maxColorValue=255),
          rgb(15,91,120,maxColorValue=255),rgb(192,46,29,maxColorValue=255), rgb(217,78,31,maxColorValue=255), rgb(241,108,32,maxColorValue=255), 
          rgb(239,139,44,maxColorValue=255),rgb(236,170,56,maxColorValue=255),rgb(235,200,68,maxColorValue=255),rgb(162,184,108,maxColorValue=255))



data_fps = data_fp %>%
  filter(Analyte == 'DO' | Analyte == 'pH' | Analyte == 'SpCond' | Analyte == 'Temp' | Analyte == 'Turb') %>%
  spread(Analyte, Result) %>%
  filter(Date >= as.Date('2018-10-18') & Date <=  as.Date('2019-04-25') )
  

data_fps

#this will give the max value for each parameter; check that there are no major outliers, correct if needed
#as.numeric(apply(data_fps, 2, max, na.rm = TRUE))

summary.dat = plyr::ddply(data_fps[,c(4:8, 1)], .(Site), summarize, 
                    mean.DO = mean(DO, na.rm = T), max.DO = max(DO, na.rm = T), min.DO = min(DO, na.rm = T),
                    mean.pH = mean(pH, na.rm = T), max.pH = max(pH, na.rm = T), min.pH = min(pH, na.rm = T),
                    mean.SpCond = mean(SpCond, na.rm = T), max.SpCond = max(SpCond, na.rm = T), min.SpCond = min(SpCond, na.rm = T),
                    mean.Temp = mean(Temp, na.rm = T), max.Temp = max(Temp, na.rm = T), min.Temp = min(Temp, na.rm = T),
                    mean.Turb = mean(Turb, na.rm = T), max.Turb = max(Turb, na.rm = T), min.Turb = min(Turb, na.rm = T))
summary.dat

windows(height=3, width=6)
par(mfrow=c(2,4), mar=c(0.6,1,0.6,1))
radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='HAT',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'HATCHET')

radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='HOGDN',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'HOGDN')

        radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='HOGNW16',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'HOGNW16')

radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='POSNW16',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'POSSUM CREEK')
radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='SWB',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'SWEETWATER')

radarchart(rbind(apply(summary.dat[,c(3,6,9,12,15)], 1, max, na.rm = T), 
                 apply(summary.dat[,c(3,6,9,12,15)], 1, min, na.rm = T),
                 summary.dat[summary.dat$Site =='TUM441',c(3,6,9,12,15)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("DO (mg/L)", "pH", "SpCond", "Temp", "Turb"), 
           pcol = colors,
           title = 'TUMBLIN CREEK')



######## Nutrient radar plot fail

nd3 = data_nut %>% 
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result)) %>%
  spread(Analyte, mean)
  

nd3

summary.dat2 = plyr::ddply(nd3[,c(3:5, 2)], .(Site), summarize, 
                           mean.NH4 = mean(NH4, na.rm = T), max.NH4 = max(NH4, na.rm = T), min.NH4 = min(NH4, na.rm = T),  
                           mean.NOx = mean(NOx, na.rm = T),max.NOx = max(NOx, na.rm = T), min.NOx = min(NOx, na.rm = T),  
                           mean.OP = mean(OP, na.rm = T), max.OP = max(OP, na.rm = T), min.OP = min(OP, na.rm = T))
summary.dat2

windows(height=3, width=6)
par(mfrow=c(2,4), mar=c(0.6,1,0.6,1))
radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='HAT',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'HATCHET')

radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='HOGDN',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'HOGDN')

radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='HOGNW16',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'HOGNW16')

radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='POSNW16',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'POSSUM CREEK')
radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='SWB',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'SWEETWATER')

radarchart(rbind(apply(summary.dat2[,c(3,6,9)], 1, max, na.rm = T), 
                 apply(summary.dat2[,c(3,6,9)], 1, min, na.rm = T),
                 summary.dat[summary.dat2$Site =='TUM441',c(3,6,9)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.2, vlcex = 1, plty = 1, 
           plwd = 2, pty = 16, cglcol = 'gray20', maxmin = T, 
           vlabels = c("NH4", "NOx", "OP"), 
           pcol = colors,
           title = 'TUMBLIN CREEK')

#####All Site Radar####

data_all = read_csv('gnv_streams_all_20190503.csv') 
  parse_date(data_all$Date, format = "%m/%d/%Y")

data_all

summary.all = plyr::ddply(data_all[,c(3:10, 2)], .(Site), summarize, 
                          mean.DO = mean(DO, na.rm = T), max.DO = max(DO, na.rm = T), min.DO = min(DO, na.rm = T),
                          mean.pH = mean(pH, na.rm = T), max.pH = max(pH, na.rm = T), min.pH = min(pH, na.rm = T),
                          mean.SpCond = mean(SpCond, na.rm = T), max.SpCond = max(SpCond, na.rm = T), min.SpCond = min(SpCond, na.rm = T),
                          mean.Temp = mean(Temp, na.rm = T), max.Temp = max(Temp, na.rm = T), min.Temp = min(Temp, na.rm = T),
                          mean.Turb = mean(Turb, na.rm = T), max.Turb = max(Turb, na.rm = T), min.Turb = min(Turb, na.rm = T),
                          mean.NH4 = mean(NH4, na.rm = T), max.NH4 = max(NH4, na.rm = T), min.NH4 = min(NH4, na.rm = T),  
                          mean.NOx = mean(NOx, na.rm = T),max.NOx = max(NOx, na.rm = T), min.NOx = min(NOx, na.rm = T),  
                          mean.OP = mean(OP, na.rm = T), max.OP = max(OP, na.rm = T), min.OP = min(OP, na.rm = T))

summary.all




windows(height=7, width=10)
par(mfrow=c(2,3), mar=c(2,2,2,2), cex.main = 2)
radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='HAT',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.4, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#56B4E9', pfcol = '#56B4E9',
           title = 'HATCHET CREEK')

radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='HOGDN',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.4, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#0072B2', pfcol = '#0072B2',
           title = 'S. HOGTOWN')

radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='HOGNW16',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.5, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#009E73', pfcol = '#009E73',
           title = 'N. HOGTOWN')

radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='POSNW16',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.5, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#669900', pfcol = '#669900',
           title = 'POSSUM CREEK')

radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='SWB',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.5, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#D55E00', pfcol = '#D55E00',
           title = 'SWEETWATER BRANCH')

radarchart(rbind(apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, max, na.rm = T), 
                 apply(summary.all[,c(3,6,9,12,15,18,21,24)], 2, min, na.rm = T),
                 summary.all[summary.all$Site =='TUM441',c(3,6,9,12,15,18,21,24)]),
           axistype = 0, axislabcol = 'gray20', palcex = 1.5, vlcex = 2, plty = 1, 
           plwd = 4.5, pty = 16, cglcol = 'gray20', maxmin = T,
           vlabels = c("DO", "pH", "SpC", "Temp", "Turb", "NH4", "NOx", "OP"), 
           pcol = '#E69F00', pfcol = '#E69F00',
           title = 'TUMBLIN CREEK')

