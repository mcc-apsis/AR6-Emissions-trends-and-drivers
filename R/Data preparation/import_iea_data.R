

rm(list = ls())
library(tidyverse)
library(openxlsx)

wb <- openxlsx::loadWorkbook("Data/Codes and classifications/iea_edgar_sector_classification.xlsx")

load("Data/iea_web.RData")

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')

countries <- iea %>%
  select(country) %>% 
  distinct()

countries <- countries %>% 
  mutate(lower=tolower(country))

countries <- left_join(countries,codes,by=c("lower"="alternative.name"))

load("Data/tsu_codes.RData")

countries <- left_join(countries,tsu_codes,by=c("alpha.3"="ISO"))
countries <- countries %>% 
  select(-lower,-name)
countries <- countries %>% 
  mutate(alpha.3=ifelse(is.na(region_ar6_5),NA,alpha.3))

countries$alpha.3[countries$country=="Chinese Taipei"] <- "TWN"         
countries$region_ar6_5[countries$country=="Chinese Taipei"] <- "Asia and Developing Pacific"
countries$region_ar6_5_short[countries$country=="Chinese Taipei"] <- "APC"

countries$alpha.3[countries$country=="Kosovo"] <- "RKS"         
countries$region_ar6_5[countries$country=="Kosovo"] <- "Developed Countries"
countries$region_ar6_5_short[countries$country=="Kosovo"] <- "DEV"

openxlsx::addWorksheet(wb,"countries")
openxlsx::writeData(wb, sheet = "countries", countries, colNames = T, rowNames = F)
openxlsx::saveWorkbook(wb,"Data/Codes and classifications/iea_edgar_sector_classification.xlsx",overwrite=T)



blarg <- iea %>% 
  filter(flow=="ONONSPEC") %>% 
  filter(country=="World")

blarg <- blarg %>% 
  filter(year==2015)
