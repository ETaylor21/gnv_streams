
library(streamMetabolizer)
library(tidyverse)




setwd('C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge')

POS = read_csv('FL_POS_2019-08-14_XX.csv')


# In POSIX, 1. designate the format to match the original date time
#  and 2. specify the timezone... a full list can be viewed by running OlsonNames()
local.time <- as.POSIXct(POS$`Date-Time`, format="%m/%d/%y %H:%M:%S", tz="EST")

UTC.time = lubridate::with_tz(local.time, tzone = 'UTC')


UTC.time

write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POS_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
