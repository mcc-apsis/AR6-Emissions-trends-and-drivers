
rm(list = ls())
library(tidyverse)
library(openxlsx)
library(janitor)

# names <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
# codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
# 
# 
# sdgs <- openxlsx::read.xlsx('Data/supplemetary data/20200505172257190_lamb@mcc-berlin.net_data.xlsx')
# 
# data <- sdgs[,1:24]
# data <- sdgs %>% 
#   clean_names()
# 
# data <- left_join(data,codes %>% select(alpha.3,numeric.code),by=c("geo_area_code"="numeric.code"))
# 
# # any countries not joined?
# missing <- anti_join(data,codes %>% select(alpha.3,numeric.code),by=c("geo_area_code"="numeric.code")) %>% 
#   select(geo_area_name) %>% 
#   distinct()
# 
# # REMOVE DATA AGGREGATED TO UN REGIONS
# data <- data %>% 
#   select(ISO=alpha.3,everything()) %>% 
#   filter(!is.na(ISO))
# 
# 
# # REMOVE INDICATORS THAT APPEAR IN MULTIPLE SDGS
# data <- data %>% 
#   distinct(ISO,series_code,time_period,value,.keep_all=TRUE)
# 
# # SUBSET DATA BY ALLAREA ONLY (REMOVE INDICATORS WITH ADDITIONAL RURAL/URBAN ROWS)
# # data <- data %>% 
# #   filter(is.na(location) | location=="ALLAREA")
# 
# # NOTE THERE ARE ADDITIONAL DIMENSIONS (AGE, SEX)
# 
# # SAVE IT ALL
# save(data,file="Data/supplemetary data/sdgs.RData")

load("Data/supplemetary data/sdgs.RData")

# list of indicators
summary <- data %>% 
  select(goal:series_description) %>% 
  distinct()

### are the time indicators different?

# time <- data %>% 
#   select(ISO,time_period,time_detail,series_description) %>% 
#   mutate(blarg = ifelse(time_period!=time_detail,1,0)) %>% 
#   filter(blarg==1)

### where are the nans? FILL OUT MISSING DATA

nans <- data %>% 
  #head(5000) %>% 
  complete(time_period=seq(2000,2019),ISO,series_description) %>% 
  group_by(series_description) %>% 
  fill(goal,target,indicator,series_code)

nans <- nans %>% 
  group_by(series_description) %>% 
  arrange(goal) %>% 
  fill(goal,target,indicator,series_code) %>% 
  arrange(goal,time_period)

nans <- nans %>% 
  group_by(goal,target,indicator,series_description,time_period,series_code) %>% 
  summarise(n=sum(!is.na(value)))

nans <- spread(nans,time_period,n)

wb <- openxlsx::createWorkbook(title = paste("sdgs_data_summary"))
openxlsx::addWorksheet(wb,"indicator_gaps")
openxlsx::writeData(wb, sheet = "indicator_gaps", nans, colNames = T, rowNames = F)
openxlsx::saveWorkbook(wb,"Results/Plots/sdgs/sdgs_data_summary.xlsx",overwrite=T)


########## make selection


selection <- openxlsx::read.xlsx('Data/supplemetary data/sdgs_data_summary_SP.xlsx',sheet="selection") %>% 
  select(series_code) %>% 
  mutate(include=1)

data <- left_join(data,selection,by=c("series_code"="series_code"))
data <- data %>% 
  filter(include==1)


data <- data %>% 
  select(goal:series_description,ISO,country=geo_area_name,time_period,time_detail,age,sex,location,source,units,value,everything()) %>% 
  filter(time_period>2014)

data %>% filter(series_code=="SI_POV_EMP1") %>% filter(sex=="BOTHSEX") %>% filter(time_period==2015) %>% 
  ggplot(.,aes(x=value,y=country)) +
  geom_point()


wb <- openxlsx::createWorkbook(title = paste("sdgs_data"))
openxlsx::addWorksheet(wb,"data")
openxlsx::writeData(wb, sheet = "data", data, colNames = T, rowNames = F)
openxlsx::saveWorkbook(wb,"Results/sdgs_data.xlsx",overwrite=T)

save(data,file="Data/sdgs_small.RData")
