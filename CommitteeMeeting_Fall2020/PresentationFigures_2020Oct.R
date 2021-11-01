##Data run for fall 2020 committee meeting on 12/09/2020

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
library(dataRetrieval)
library(patchwork)
library(wesanderson)
library(viridis)
library(hrbrthemes)
library(cowplot)
options(scipen = 999)
library(MaizePal) 


#set working directory

setwd('C:/Users/Emily/Documents/gnv_streams/CommitteeMeeting_Fall2020')

#call in data file

fp <- read_csv('gnvstreams_fp_20201109.csv')

#####Field Parameter Time Series####

#windows()

fp2 = fp %>% 
  filter(Analyte == 'Dissolved Oxygen Saturation' | Analyte == 'pH' | Analyte == 'Specific Conductance' | Analyte == 'Temperature, Water' | Analyte == 'Turbidity') %>% 
  filter(Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'SWB' | Site == 'SWBUP' | Site == 'POSNW16' | Site == 'TUM441' ) %>% 
  mutate(Site = factor(Site, levels =  c('HOGDN', 'HOGNW16', 'POSNW16', 'SWBUP', 'SWB', 'TUM441'))) %>% 
  group_by(Date, Site, Analyte) 

fp2

field_param_ts = ggplot(fp2, aes(x = Date, y = Result))

fpts <- field_param_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(`Dissolved Oxygen Saturation` = "DO Saturation (%)", 
                                       `pH` = 'pH (SU)', 
                                       `Specific Conductance` = 'SpCond (uS/cm)', 
                                       `Temperature, Water` = 'Temperature (degC)', 
                                       Turbidity = 'Turbidity (NTU)' )))  
  

fpts <- fpts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                          labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(strip.placement = 'outside', strip.text = element_text(size = 11), 
        axis.text = element_text(size = rel(1.1)), 
        axis.title.x = element_blank(),
        legend.position = 'bottom')

fpts

#ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/fpts_20Nov09.png", plot = fpts, width = 14, height = 12)


#####Nutrient Time-Series - ALL####

nut <- read_csv('gnv_streams_nut_20201109.csv')

nut2 = nut %>% 
  filter(Analyte == 'Ammonia (N)' | Analyte == 'Nitrate-Nitrite (N)' | Analyte == 'Orthophosphate (P)') %>% 
  filter(Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'SWB' | Site == 'SWBUP' | Site == 'POSNW16' | Site == 'TUM441' ) %>% 
  mutate(Site = factor(Site, levels =  c('HOGDN', 'HOGNW16', 'POSNW16', 'SWBUP', 'SWB', 'TUM441'))) %>% 
  group_by(Date, Site, Analyte) 

nut2

nutrient_ts = ggplot(nut2, aes(x = Date, y = Result))

nutts <- nutrient_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  facet_wrap( . ~ Analyte , scales = 'free_y', nrow = 5, 
              strip.position = 'left',
              labeller = as_labeller(c(`Ammonia (N)` = "Ammonia (mg N/L)", 
                                       `Nitrate-Nitrite (N)` = 'Nitrate-Nitrite (mg N/L)', 
                                       `Orthophosphate (P)` = 'Orthophosphate (mg P/L)' )))  


nutts <- nutts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                                  labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(strip.placement = 'outside', strip.text = element_text(size = 11), 
        axis.text = element_text(size = rel(1.1)), 
        axis.title.x = element_blank(),
        legend.position = 'bottom')

nutts

#####Nutrient Time-Series - NOx####

NOX = nut %>% 
  filter(Analyte == 'Nitrate-Nitrite (N)') %>% 
  filter(Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'SWB' | Site == 'SWBUP' | Site == 'POSNW16' | Site == 'TUM441' ) %>% 
  mutate(Site = factor(Site, levels =  c('HOGDN', 'HOGNW16', 'POSNW16', 'SWBUP', 'SWB', 'TUM441'))) %>% 
  group_by(Date, Site, Analyte) 

NOX

NOX_ts = ggplot(NOX, aes(x = Date, y = Result))

NOXts <- NOX_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  labs(y = 'Nitrate-Nitrite (mg N/L)')  


NOXts <- NOXts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                                    labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom')

NOXts

ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/NOXts_20Nov09.png", plot = NOXts, width = 18, height = 12)


#####Nutrient Time-Series - NOx - log-transformed####

NOXlogts <- NOX_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  scale_y_log10() +
  labs(caption = 'Note: x-axis in log-scale', 
       y = 'Nitrate-Nitrite (mg N/L)')  


NOXlogts <- NOXlogts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                                    labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom')

NOXlogts

ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/NOXts_log_20Nov09.png", plot = NOXlogts, width = 18, height = 12)


#####Nutrient Time-Series - NH4####

NH = nut %>% 
  filter(Analyte == 'Ammonia (N)') %>% 
  filter(Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'SWB' | Site == 'SWBUP' | Site == 'POSNW16' | Site == 'TUM441' ) %>% 
  filter(Date >= as.Date('2019-05-07') & Date <=  as.Date('2020-09-10')) %>% 
  filter(Result > 0.004) %>% 
  mutate(Site = factor(Site, levels =  c('HOGDN', 'HOGNW16', 'POSNW16', 'SWBUP', 'SWB', 'TUM441'))) %>% 
  group_by(Date, Site, Analyte) 

NH

NH_ts = ggplot(NH, aes(x = Date, y = Result))

NHts <- NH_ts + 
  geom_line(aes(group = Site, color = Site)) + 
  ylab(NULL) + 
  scale_y_log10() +
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  labs(y = 'Ammonia (mg N/L)')  


NHts <- NHts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                                    labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom')

NHts

ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/NHts_ALL_20Nov09.png", plot = NHts, width = 18, height = 12)

#####Nutrient Time-Series - OP####

OP = nut %>% 
  filter(Analyte == 'Orthophosphate (P)') %>% 
  filter(Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'SWB' | Site == 'SWBUP' | Site == 'POSNW16' | Site == 'TUM441' ) %>% 
  filter(Date >= as.Date('2019-02-06') & Date <=  as.Date('2020-09-10')) %>% 
  mutate(Site = factor(Site, levels =  c('HOGDN', 'HOGNW16', 'POSNW16', 'SWBUP', 'SWB', 'TUM441'))) %>% 
  group_by(Date, Site, Analyte) 

OP

OP_ts = ggplot(OP, aes(x = Date, y = Result))

OPts <- OP_ts + geom_line(aes(group = Site, color = Site)) + ylab(NULL) + 
  geom_point(size = 4, aes(x = Date, color = Site, shape = Site)) + 
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  labs(y = 'Orthophosphate (mg P/L)')  


OPts <- OPts + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900',  '#D55E00', '#E69F00'), 
                                  labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) +
  scale_shape_discrete(labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'N. Sweetwater', 'Sweetwater', 'Tumblin')) + 
  guides(fill = FALSE, col = guide_legend(nrow = 1)) +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  background_grid(major = 'xy', minor = 'none', size.major = 0.5) +
  theme(axis.title.x = element_blank(),
        legend.position = 'bottom')

OPts

ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/OPts_20Nov09.png", plot = OPts, width = 18, height = 12)


OPts/NOXts/NHts
