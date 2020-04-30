rm(list = ls())
library(tidyverse)
library(openxlsx)

blue <- read.xlsx("Data/Land and GCB/Country_ELUC_11102019_peatAdded_wAvgAndUnc.xlsx",sheet = "(BLUE_GCB2019_withPeat_1Sudan)")

blue <- gather(blue,country,value,Afghanistan:Others) %>% 
  select(year = X1,country,value)

# totals <- blue %>% 
#   group_by(year) %>% 
#   summarise(value=sum(value))

# units are TgC. 

blue <- blue %>% 
  mutate(value=value*1e6) %>% 
  mutate(value=value*3.664)

# join ISO codes
codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
blue <- left_join(blue %>% mutate(country=tolower(country)),codes,by=c("country"="alternative.name"))

uhoh <- anti_join(blue %>% mutate(country=tolower(country)),codes,by=c("country"="alternative.name")) %>% 
  select(country) %>% 
  distinct()


load('Data/edgar_data_gwp_ar5.RData')

edgar_countries <- edgar_GHG_ar5 %>% 
  group_by(country,ISO,year) %>% 
  summarise(CO2=sum(CO2,na.rm=T),CH4=sum(CH4,na.rm=T),N2O=sum(N2O,na.rm=T))

blarg <- left_join(edgar_countries,blue %>% select(-country),by=c("year"="year","ISO"="alpha.3"))

blarg <- blarg %>% 
  filter(ISO=="USA")
