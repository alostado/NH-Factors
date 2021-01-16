library(tibbletime)
library(tidyverse)
library(broom)
library(lubridate)
library(RColorBrewer)
theme_set(theme_minimal())

load('CTAdb.RData')

roll_capm <- rollify(.f = function(y,Mkt.RF){
  lm(y ~ Mkt.RF)},
                     window = 12, # 12 months = 1 year
                     unlist = F)

capm = df %>% 
  filter(between(Date, capm_mindate, capm_maxdate)) %>% # last capm observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= 12, sum(is.na(Return)) == 0) %>% 
  # at least 12 observations, exclude series with missing values
  mutate(
    model = 'CAPM',
    fit = roll_capm(Return-RF, Mkt.RF)
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy)) %>%
  select(-fit) %>% unnest(tidied)


roll_ff3 <- rollify(
  .f = function(y,Mkt.RF,SMB,HML){
    lm(y ~ Mkt.RF+SMB+HML)},
  window = 12, # 12 months = 1 year
  unlist = F)

ff3 = df %>% 
  filter(between(Date, ff3_mindate, ff3_maxdate)) %>% # last ff3 observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= 12, sum(is.na(Return)) == 0) %>%
  mutate(
    model = 'FF3',
    fit = roll_ff3(Return-RF, Mkt.RF, SMB, HML)
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy)) %>%
  select(-fit) %>% unnest(tidied)

roll_ff5 <- rollify(
  .f = function(y,Mkt.RF,SMB,HML,RMW,CMA){
    lm(y ~ Mkt.RF+SMB+HML+RMW+CMA)},
  window = 12, # 12 months = 1 year
  unlist = F)

ff5 = df %>% 
  filter(between(Date, ff5_mindate, ff5_maxdate)) %>% # last ff5 observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= 12, sum(is.na(Return)) == 0) %>%
  mutate(
    model = 'FF5',
    fit = roll_ff5(Return-RF, Mkt.RF, SMB, HML, RMW, CMA)
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy)) %>%
  select(-fit) %>% unnest(tidied)

roll_ff5_mom <- rollify(
  .f = function(y,Mkt.RF,SMB,HML,RMW,CMA,MOM){
    lm(y ~ Mkt.RF+SMB+HML+RMW+CMA+MOM)},
  window = 12, # 12 months = 1 year
  unlist = F)

ff5_mom = df %>% 
  filter(between(Date, ff5_mom_mindate, ff5_mom_maxdate)) %>% # last MOM observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= 12, sum(is.na(Return)) == 0) %>%
  mutate(
    model = 'FF5+MOM',
    fit = roll_ff5_mom(Return-RF, Mkt.RF, SMB, HML, RMW, CMA, MOM)
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy)) %>%
  select(-fit) %>% unnest(tidied)

roll_lbs <- rollify(
  .f = function(y,PTFSBD,PTFSFX,PTFSCOM,PTFSIR,PTFSSTK){
    lm(y ~ PTFSBD+PTFSFX+PTFSCOM+PTFSIR+PTFSSTK)},
  window = 12, # 12 months = 1 year
  unlist = F)

lbs = df %>% 
  filter(between(Date, lbs_mindate, lbs_maxdate)) %>% # last lbs observation
  group_by(ID, Program, Manager, Type, Style, Strategy, Sector) %>%
  filter(sum(!is.na(Return)) >= 12, sum(is.na(Return)) == 0) %>%
  mutate(
    model = 'LBS',
    fit = roll_lbs(Return-RF, PTFSBD, PTFSFX, PTFSCOM, PTFSIR, PTFSSTK)
  ) %>% 
  filter(!is.na(fit)) %>%
  mutate(tidied = map(fit, tidy)) %>%
  select(-fit) %>% unnest(tidied)


rm(list=setdiff(ls(), c('capm', 'ff3', 'ff5', 'ff5_mom', 'lbs')))

save.image('rolling_results.RData')


