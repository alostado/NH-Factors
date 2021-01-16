library(tidyverse)
library(readxl)

download.file(
  "https://nordichedge.com/Database/CTADatabase.csv",
  "factordata/CTADatabase.csv"
)
download.file(
  "https://nordichedge.com/Database/CTADatabaseClassification.csv",
  "factordata/CTADatabaseClassification.csv"
)

temp <- tempfile()
download.file(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
  temp
)
unzip(temp, exdir='factordata')

dat <- readLines('factordata/F-F_Research_Data_5_Factors_2x3.CSV')

keep <- as.numeric(substr(dat, 1, 6))
keep[is.na(keep)] <- 0
keep <- keep>10000
dat <- dat[keep]
dat <- read.csv(textConnection(dat), header = F)
colnames(dat) <- c('yearmon', 'Mkt.RF', 'SMB', 'HML', 'RMW', 'CMA', 'RF')
write.table(dat, 'factordata/ff5.csv', sep = ',', row.names = F, col.names = T)


download.file(
  'http://faculty.fuqua.duke.edu/~dah7/DataLibrary/TF-Fac.xls',
  'factordata/lbs.xls',
  mode='wb'
)

download.file(
  'https://images.aqr.com/-/media/AQR/Documents/Insights/Data-Sets/AQR-Index-Returns.xls',
  'factordata/aqr_mom.xls',
  mode='wb'
)


rm(list = ls())

CTAdb <- tibble(read.csv('factordata/CTADatabase.csv', header = F))
colnames(CTAdb) <- c('Manager', 'Program', 'Date', 'Return', 'AUM')

CTAclass <- tibble(read.csv('factordata/CTADatabaseClassification.csv', 
                            header = F))
colnames(CTAclass) <- c('Manager', 'Program', 'Type', 'Style', 'Strategy', 'Sector')

CTAdb$Date <- as.Date(CTAdb$Date)
CTAdb$yearmon <- format(CTAdb$Date, format = '%b %Y')

CTAdb$Manager[CTAdb$Manager == 'Rcube Asset Management'] = 'RCube Asset Management'

CTAclass = CTAclass %>% mutate(ID = row_number(Program))

CTAdb = left_join(CTAdb, CTAclass, by=c('Program','Manager'))

# FF5 factors

ff5_data_monthly <- tibble(read.csv('factordata/ff5.csv'))


ff5_data_monthly$yearmon <- 
  format(as.Date(paste0(as.character(ff5_data_monthly$yearmon),'01'), 
                 '%Y%m%d'), 
         format='%b %Y')

ff5_data_monthly = ff5_data_monthly %>% 
  mutate(across(-yearmon, function(x){x/100}))

# LBS factors

lbs_data_monthly = tibble(read_excel('factordata/lbs.xls'))

keep <- !is.na(as.numeric(as.matrix(lbs_data_monthly[,1])))

lbs_data_monthly <- lbs_data_monthly[keep,]

colnames(lbs_data_monthly) <- c('yearmon', 'PTFSBD',	'PTFSFX',	
                                'PTFSCOM',	'PTFSIR',	'PTFSSTK')

lbs_data_monthly$yearmon <- 
  format(as.Date(paste0(as.character(lbs_data_monthly$yearmon),'01'), 
                 '%Y%m%d'), 
         format='%b %Y')

lbs_data_monthly = lbs_data_monthly %>% 
  mutate(across(-yearmon, as.numeric))


# AQR momentum factor

aqr_mom = tibble(read_excel("factordata/aqr_mom.xls", sheet = "Returns", 
                            skip = 1))

colnames(aqr_mom)[1:2] = c('yearmon', 'MOM')

aqr_mom = aqr_mom %>% select(c(yearmon, MOM)) %>%
  mutate(yearmon = format(yearmon, format='%b %Y'))


# Join data with factors

df <- left_join(CTAdb, ff5_data_monthly, by='yearmon')

df <- left_join(df, aqr_mom, by='yearmon')

df <- left_join(df, lbs_data_monthly, by='yearmon')


# Get limit dates for factor models

capm_logic = !is.na(df$RF) & !is.na(df$Mkt.RF)
ff3_logic = capm_logic & !is.na(df$SMB) & !is.na(df$HML)
ff5_logic = ff3_logic & !is.na(df$RMW) & !is.na(df$CMA)
ff5_mom_logic = ff5_logic & !is.na(df$MOM)
lbs_logic = !is.na(df$RF) & !is.na(df$PTFSBD) & !is.na(df$PTFSFX) & 
  !is.na(df$PTFSCOM) & !is.na(df$PTFSIR) & !is.na(df$PTFSSTK)

capm_mindate = min(df$Date[capm_logic])
capm_maxdate = max(df$Date[capm_logic])
ff3_mindate = min(df$Date[ff3_logic])
ff3_maxdate = max(df$Date[ff3_logic])
ff5_mindate = min(df$Date[ff5_logic])
ff5_maxdate = max(df$Date[ff5_logic])
ff5_mom_mindate = min(df$Date[ff5_mom_logic])
ff5_mom_maxdate = max(df$Date[ff5_mom_logic])
lbs_mindate = min(df$Date[lbs_logic])
lbs_maxdate = max(df$Date[lbs_logic])

rm(list=setdiff(ls(), c('df', 'capm_mindate','capm_maxdate','ff3_mindate',
                'ff3_maxdate','ff5_mindate','ff5_maxdate','ff5_mom_mindate',
                'ff5_mom_maxdate','lbs_mindate','lbs_maxdate')))

save.image('CTAdb.RData')
