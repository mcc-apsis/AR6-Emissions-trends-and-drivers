rm(list = ls())
library(zoo)
library(openxlsx)
library(tidyverse)


ipcc_regions <- read.xlsx('Data/Codes and classifications/Country categories plus alpha codes 27-04-2021.xlsx','Breakdown_list_dev_level')

ipcc_regions <- ipcc_regions %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Developed Countries","DEV",NA)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Latin America and Caribbean","LAM",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Africa","AFR",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Middle East","MEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Eastern Europe and West-Central Asia","EEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Asia and Developing Pacific","APC",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Shipping","SEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Aviation","AIR",region_ar6_5_short))

write.xlsx(ipcc_regions,file="Results/Codes and classifications/IPCC_regions.xlsx",sheetName="region_classification",row.names = FALSE)

save(ipcc_regions,file='Data/ipcc_regions.RData')
