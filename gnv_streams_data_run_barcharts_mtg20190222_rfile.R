#Data run for lab meeting on 02/22/2019
###########Making BAR CHARTS!!!!###########

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

#set working directory

setwd('C:/Users/etaylor21/Documents/gnv_streams')

#call in nutrient data file

data_nut_bar=read_csv('gnv_streams_data_run_mtg20190222.csv')

data_fp_bar=read_csv('gnv_streams_data_run_mtg20190222_v2.csv')

data_fp_bar = drop_na(data_fp_bar,)

######Get POR average of analytes by station###############

group_data_nut_bar = group_by(data_nut_bar, Site, Analyte)#by which columns am I grouping my data by
group_data_nut_bar#check the groups are correct
data_nut_bar_USE_ME = summarise(group_data_nut_bar, mean = mean(Result), sd = sd(Result))#calculate average of result for analyte test
data_nut_bar_USE_ME

group_data_fp_bar = group_by(data_fp_bar, Site, Analyte)#by which columns am I grouping my data by
group_data_fp_bar#check the groups are correct
data_fp_bar_USE_ME = summarise(group_data_fp_bar, mean = mean(Result), sd = sd(Result))#calculate average of result for analyte test
data_fp_bar_USE_ME

############create barcharts with error bar###############

nutrient = ggplot(data_nut_bar_USE_ME, aes(x = Site, y = mean, fill = Analyte))

p = nutrient + geom_col(position = 'dodge') + ylab('Result (mg/L)')

p + geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(0.9)) + facet_grid(Analyte ~ ., scales = 'free_y')


fp = ggplot(data_fp_bar_USE_ME, aes(x = Site, y = mean, fill = Analyte))

pf = fp + geom_col(position = 'dodge') + ylab('Result')

pf + geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2, position = position_dodge(0.9)) + facet_grid(Analyte ~ ., scales = 'free_y')
                  