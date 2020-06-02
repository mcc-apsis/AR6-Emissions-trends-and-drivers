
rm(list = ls())
library(tidyverse)
library(openxlsx)

names <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')


act <- read.xlsx('Data/IEA/Energyefficiencyindicators-extended.xlsm',sheet=7,startRow = 2)
act <- gather(act,year,value,`2000`:`2017`) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(year=as.numeric(year))

act <- left_join(act %>% mutate(Country=tolower(Country)),names,by=c("Country"="alternative.name"))
missing <- anti_join(act %>% mutate(Country=tolower(Country)),names,by=c("Country"="alternative.name"))
act <- left_join(act,codes %>% select(name,alpha.3),by=c("alpha.3"="alpha.3"))

load('Data/tsu_codes.RData')
act <- left_join(act,tsu_codes %>% select(ISO,region_ar6_5,region_ar6_5_short),by=c("alpha.3"="ISO"))

act <- act %>% 
  select(-Country) %>% 
  select(country=name,ISO=alpha.3,region_ar6_5,region_ar6_5_short,activity=Activity,product=Product,year,value)


act$product <- sub('[ \t]+$','',act$product)
act$activity <- sub('[ \t]+$','',act$activity)


act <- act %>% 
  mutate(pop=ifelse(product=="Population (10^6)",value,NA)) %>% 
  group_by(country,year) %>% 
  mutate(pop=first(pop)) %>% 
  ungroup() %>% 
  filter(product!="Population (10^6)")

act <- act %>% 
  select(country,ISO,region_ar6_5,region_ar6_5_short,year,pop,activity,product,value)


save(act,file='Data/activity.RData')





# residential <- read.xlsx('Data/IEA/Energyefficiencyindicators-extended.xlsm',sheet=3,startRow = 2)
# 
# services <- read.xlsx('Data/IEA/Energyefficiencyindicators-extended.xlsm',sheet=3,startRow = 2)
# 
# industry <- read.xlsx('Data/IEA/Energyefficiencyindicators-extended.xlsm',sheet=3,startRow = 2)
# 
# transport <- read.xlsx('Data/IEA/Energyefficiencyindicators-extended.xlsm',sheet=3,startRow = 2)
