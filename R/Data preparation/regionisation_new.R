rm(list = ls())
library(zoo)
library(openxlsx)
library(tidyverse)


tsu_codes <- read.xlsx('Data/Codes and classifications/Country categories plus alpha codes 27-11-2019.xlsx','Breakdown_list_dev_level')

tsu_codes <- tsu_codes %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Developed Countries","DEV",NA)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Latin America and Caribbean","LAM",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Africa and Middle East","AME",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Eastern Europe and West-Central Asia","EEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Asia and Developing Pacific","APC",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Shipping","SEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Aviation","AIR",region_ar6_5_short))

write.xlsx(tsu_codes,file="Results/Codes and classifications/IPCC_regions.xlsx",sheetName="regions",row.names = FALSE)

save(tsu_codes,file='Data/tsu_codes.RData')
