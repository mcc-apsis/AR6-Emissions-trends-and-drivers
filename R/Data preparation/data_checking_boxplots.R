rm(list = ls())
library(tidyverse)

#load('../../Data/edgar_data_gwp_ar6.RData')
load("../../Data/edgar_data_all.RData")
load('../../Data/gwps.RData')
#load("../../Data/land.RData")


GHG_data<-edgar_GHG %>% filter(year==2019)


# GHG_data<-GHG_data %>%
#   group_by(year,region_ar6_10,chapter_title) %>%
#   summarise_at(vars(CO2:SF6),sum,na.rm=TRUE) %>% 
#   mutate_at(vars(CO2:SF6),list(~./1e9))

GHG_data <- gather(GHG_data,gas,value,CO2:SF6)

methane <- GHG_data %>% 
  filter(gas=="CH4") 

GHG_data <- left_join(GHG_data,gwps,by = "gas") %>%
  select(-gwp_ar2,-gwp_ar4,-gwp_ar5,-gwp_ar6_old) %>%
  mutate(gwp_ar6=gwp_ar6*value) %>%
  select(-value)

#### recalculate methane in AR6 using biogenic / non-biogenic methane GWPs


#methane <- methane %>% 
#  group_by(year,region_ar6_10,chapter_title,sector_code) %>% 
#  summarise(CH4=sum(CH4,na.rm=TRUE))

methane <- left_join(methane,gwps_ch4 %>% select(sector_code,"gwp"=value),by = "sector_code")
############################## continue
methane <- methane %>% 
  mutate(CH4_adjusted=value*gwp) %>%
  group_by(year,region_ar6_10,chapter_title) %>%
  summarise(CH4_adjusted=sum(CH4_adjusted)/1e9) %>% 
  mutate(gas="CH4")

GHG_data <- left_join(GHG_data,methane,by = c("year", "gas","region_ar6_10","chapter_title"))

GHG_data <- GHG_data %>% 
  mutate(gwp_ar6=ifelse(!is.na(CH4_adjusted),CH4_adjusted,gwp_ar6)) %>% 
  select(-CH4_adjusted)

#### calculate fgas and land emissions

fgas <- GHG_data %>% 
  filter(!gas %in% c("CO2","CH4","N2O")) %>% 
  group_by(year,region_ar6_10,chapter_title) %>% 
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>% 
  mutate(gas="Fgas") %>% 
  select(year,gas,everything())

land_totals <- land %>% 
  filter(year>1969) %>% 
  filter(year<2020) %>% 
  mutate(gwp_ar6=mean/1e9) %>%
  mutate(gas="CO2 Land use") %>% 
  mutate(chapter_title="AFOLU") %>%
  select(year,gas,region_ar6_10,chapter_title,gwp_ar6)

GHG_data <- GHG_data %>% 
  filter(gas %in% c("CO2","CH4","N2O"))
GHG_data <- rbind(GHG_data,fgas)
GHG_data <- rbind(GHG_data,land_totals) %>% 
  mutate(gas=ifelse(gas=="CO2","CO2 FFI",gas))
