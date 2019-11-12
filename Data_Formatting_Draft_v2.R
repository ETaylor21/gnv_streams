
library(streamMetabolizer)
library(tidyverse)
library(readxl)

###for home computer###
setwd('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge')

###for work computer###
#setwd('C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge')

#####County Data####
####POSSUM CREEK DATA CLEAN UP####
#Data arrived in a highly complicated xlsx file. The first sheet in the file was duplicated and extraneous data was removed. 
#The date-time column and the discharge columns presevered, the date-time format was fixed to match m/d/Y H:M:S and the files was saved at .cvs
#dates and times from the county are in Eastern Daylights Savings Time so must use New York Time Zone
POS = read_csv('FL_POSNW16_2019-08-14_XX.csv', col_types = cols(
  `Date-Time` = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
  `Discharge (ft3/s)` = col_double()))

POS

tz(POS$`Date-Time`)


POS <- POS %>%
  mutate(`Date-Time`= force_tz(`Date-Time`, "America/New_York"))

POS

tz(POS$`Date-Time`)

diff(POS$`Date-Time`)

POS <- POS %>%
  mutate(`Date-Time` = lubridate::as_datetime(`Date-Time`, tz = 'UTC'))

POS$`Date-Time`

POS$`Discharge (ft3/s)` = POS$`Discharge (ft3/s)` * 0.0283 

POS = rename(POS, `Discharge (m3/s)` = `Discharge (ft3/s)`, `Date-Time (UTC)` = `Date-Time`)

#write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POS_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
write_csv(POS, path = 'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POSNW16_2019-08-14_XX.csv', 
          na = 'NA', append =FALSE, col_names = TRUE)



####TUM CREEK DATA CLEAN UP####
#Data arrived in a highly complicated xlsx file. The first sheet in the file was duplicated and extraneous data was removed. 
#The date-time column and the discharge columns presevered, the date-time format was fixed to match m/d/Y H:M:S and the files was saved at .cvs
#dates and times from the county are in Eastern Daylights Savings Time so must use New York Time Zone
TUM = read_csv('FL_TUM441_2019-08-14_XX.csv', col_types = cols(
  `Date-Time` = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
  `Discharge (ft3/s)` = col_double()))

TUM


tz(TUM$`Date-Time`)

TUM <- TUM %>%
  mutate(`Date-Time`= force_tz(`Date-Time`, "America/New_York"))

TUM

tz(TUM$`Date-Time`)

diff(TUM$`Date-Time`)

TUM <- TUM %>%
  mutate(`Date-Time` = lubridate::as_datetime(`Date-Time`, tz = 'UTC'))

TUM$`Date-Time`

TUM$`Discharge (ft3/s)` = TUM$`Discharge (ft3/s)` * 0.0283 

TUM = rename(TUM, `Discharge (m3/s)` = `Discharge (ft3/s)`, `Date-Time (UTC)` = `Date-Time`)

#write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_TUM_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
write_csv(TUM, path = 'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_TUM441_2019-08-14_XX.csv', 
          na = 'NA', append =FALSE, col_names = TRUE)



#####HOGNW16 DATA CLEAN UP#####

#Data arrived in a highly complicated xlsx file. The first sheet in the file was duplicated and extraneous data was removed. 
#The date-time column and the discharge columns preserved, the date-time format was fixed to match m/d/Y H:M:S and the files was saved at .cvs
#dates and times from the county are in Eastern Daylights Savings Time so must use New York Time Zone
HOGNW16 = read_csv('FL_HOGNW16_2019-08-14_XX.csv', col_types = cols(
  `Date-Time` = col_datetime(format = "%m/%d/%Y %H:%M:%S"),
  `Discharge (ft3/s)` = col_double()))

HOGNW16

tz(HOGNW16$`Date-Time`)

HOGNW16 <- HOGNW16 %>%
  mutate(`Date-Time`= force_tz(`Date-Time`, "America/New_York"))

HOGNW16

tz(HOGNW16$`Date-Time`)

diff(HOGNW16$`Date-Time`)

HOGNW16 <- HOGNW16 %>%
  mutate(`Date-Time` = lubridate::as_datetime(`Date-Time`, tz = 'UTC'))

HOGNW16$`Date-Time`

HOGNW16$`Discharge (ft3/s)` = HOGNW16$`Discharge (ft3/s)` * 0.0283 

HOGNW16 = rename(HOGNW16, `Discharge (m3/s)` = `Discharge (ft3/s)`, `Date-Time (UTC)` = `Date-Time`)

#write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HOGNW16_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
write_csv(HOGNW16, path = 'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HOGNW16_2019-08-14_XX.csv', 
          na = 'NA', append =FALSE, col_names = TRUE)







#####SJRWMD Data####
#district data are in Eastern Standard Time (no daylights saving) so you should use EST instead of New York for inital time zone

#####Hatchet Creek####
#these data were in a combined spreadsheet with HOGDN data. Data were seperated into two new files. Extraneous columns were removed (i.e quality codes)

#HAT <- read_xlsx("FL_HAT_2019-09-01_XX.xlsx", col_names = TRUE, col_types = cols(`Date` = col_date(format = '%m/%d/%Y'),`Time` = col_time(format = '%T'),`Flow (cfs)` = col_double()))

HAT <- read_xlsx("FL_HAT_2019-09-01_XX.xlsx")

HAT

diff(HAT$Time)

HAT <- HAT %>%
  mutate(datetime = paste(HAT$Date, format(HAT$Time, "%T")) %>% ymd_hms(tz = "EST")) %>%
  select(datetime, `Flow (cfs)`)  # drop date/time cols

HAT

HAT$datetime

tz(HAT$datetime)

diff(HAT$datetime)

HAT <- HAT %>%
  mutate(datetime = lubridate::as_datetime(datetime, tz = 'UTC'))

HAT$datetime

HAT$`Flow (cfs)` = HAT$`Flow (cfs)` * 0.0283 

HAT = rename(HAT, `Discharge (m3/s)` = `Flow (cfs)`, `Date-Time (UTC)` = datetime)

#write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POS_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
write_csv(HAT, path = 'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HAT_2019-09-01_XX.csv', 
          na = 'NA', append =FALSE, col_names = TRUE)


#####HOGDN Creek####
#these data were in a combined spreadsheet with HAT data. Data were separated into two new files. Extraneous columns were removed (i.e quality codes)

#HOGDN <- read_xlsx("FL_HOGDN_2019-09-01_XX.xlsx", col_names = TRUE, col_types = cols(`Date` = col_date(format = '%m/%d/%Y'),`Time` = col_time(format = '%T'),`Flow (cfs)` = col_double()))

HOGDN <- read_xlsx("FL_HOGDN_2019-09-01_XX.xlsx")

HOGDN

diff(HOGDN$Time)

HOGDN <- HOGDN %>%
  mutate(datetime = paste(HOGDN$Date, format(HOGDN$Time, "%T")) %>% ymd_hms(tz = "EST")) %>%
  select(datetime, `Flow (cfs)`)  # drop date/time cols

HOGDN

HOGDN$datetime

tz(HOGDN$datetime)

diff(HOGDN$datetime)

HOGDN <- HOGDN %>%
  mutate(datetime = lubridate::as_datetime(datetime, tz = 'UTC'))

HOGDN$datetime

HOGDN$`Flow (cfs)` = HOGDN$`Flow (cfs)` * 0.0283 

HOGDN = rename(HOGDN, `Discharge (m3/s)` = `Flow (cfs)`, `Date-Time (UTC)` = datetime)

#write_excel_csv(UTC.time, path = 'C:/Users/etaylor21/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POS_2019-08-14_XX_v2.csv', na = 'NA', col_names = TRUE)
write_csv(HOGDN, path = 'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HOGDN_2019-09-01_XX.csv', 
          na = 'NA', append =FALSE, col_names = TRUE)
