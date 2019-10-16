##### Violin Plots of NOx and SpC For AJ NSF Proposal 10/16/19####

####NOx, O_P, NH4 all filtered samples###

#NOx - Nitrate(NO3)-Nitrite(NO2)-N (mg/L)
#SpCond - specific conductivity (uS/cm)


####Sample Replications (col = Rep) 
#a = 1; b = 2; c = 3; d = not a rep just a reading

########################Call in and format data########################################

#call in packages
library(tidyverse)
library(lubridate)
library(RColorBrewer)

#set working directory

setwd('C:/Users/Emily/Documents/gnv_streams/Misc. Charts')

#call in data file

data_nut = read_csv('gnv_nutspdata_20191016.csv', col_types = cols(
  Site = col_character(),
  Date = col_date(format = "%m/%d/%Y"),
  Analyte = col_character(),
  Result = col_double()))#nutrient data; fixed date format for date column\

#####NOx and SpC Violin Plots (nvp)####

nd2 = data_nut %>% 
  group_by(Date, Site, Analyte) %>%
  summarise(mean = mean(Result))

windows()
nvp = ggplot(nd2, aes(x = Site, y = mean, fill = Analyte)) +
  geom_violin(scale = 'count', adjust = 0.5) + ylab('Results (uS/cm)                                                Results (mg/L)')

nvp2 = nvp + scale_x_discrete(labels = c('Hatchet', 'N. Hogtown', 'S. Hogtown', 'Possum', 'Sweetwater', 'Tumblin'))#change the names on the x axis, use discrete since non-numeric values

nvp3 = nvp2 + facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 2) +
  theme(strip.text = element_text(size = 18)) +
  scale_fill_manual(values = c('#56B4E9','#D55E00', '#009E73' )) + guides(fill = FALSE) +
  theme(axis.text = element_text(size = rel(1.2))) + 
  theme(axis.title = element_text(size = rel(1.3))) +
  theme(axis.title.x = element_blank())


nvp3
