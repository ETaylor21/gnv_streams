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

data.nut=read_csv('gnv_streams_data_run_mtg20190222.csv')

data.fp=read_csv('gnv_streams_data_run_mtg20190222_v2.csv')

data.fp = drop_na(data.fp)

data.nut
data.fp

############grouping data for nutrient figures#############

group.data.nut = group_by(data.nut, Site, Analyte)#by which columns am I grouping my data by
group.data.nut
summarise(group.data.nut)

data.nut.bar.USE.ME = summarise(group.data.nut, mean = mean(Result), std.error = sqrt(var(Result)/length(Result)))#calculate average of result for analyte test and standard error
data.nut.bar.USE.ME

#####Nutrient Bar Chart plotting############

nutrient = ggplot(data.nut.bar.USE.ME, aes(x = Site, y = mean, fill = Analyte))

p = nutrient + geom_col(position = 'dodge') + ylab('Results (mg/L)') #make column chart

pf = p + geom_errorbar(aes(ymin = mean-std.error, ymax = mean+std.error), width = 0.2, position = position_dodge(0.9)) + 
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 3, 
              labeller = as_labeller(c(NH4 = "Ammonium (N)", NOx = "Nitrate-Nitrite (N)", OP = 'Orthophosphate (P)')))#adding error bars to column chart and creating facets; 
  #in facet wrap the first changes whether panels are arranged horizantally or vertically, second allows sites to be different on the y, 
  #third changes the amont of rows to 3 and columns to 0 so that figures stack and headers stay on top

pf + scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73' )) + guides(fill = FALSE)#changes colors on bar fill to colorblind friendly


