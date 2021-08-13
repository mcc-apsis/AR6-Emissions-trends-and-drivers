# aviation trends


rm(list = ls())
library(tidyverse)
load('Data/edgar5_data_ghg_gwp_ar6.RData')

data <- edgar_ghg  %>% 
  mutate(version="EDGARv5_FT") %>% 
  filter(chapter_title=="Transport") %>% 
  group_by(year,subsector_title,version) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE))

data1 <- data %>% 
  filter(year %in% c(1990,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_1990_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_1990_2018=rate_1990_2018*100) %>% 
  filter(year==2018)

data2 <- data %>% 
  filter(year %in% c(2010,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_2010_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_2010_2018=rate_2010_2018*100) %>% 
  filter(year==2018)

aviation_summary <- left_join(data1,data2, by = c("year", "subsector_title", "GHG","version"))


##############

load('Data/edgar6_v2_data_ghg_gwp_ar6.RData')

data <- edgar_ghg  %>% 
  mutate(version="EDGARv6") %>% 
  filter(chapter_title=="Transport") %>% 
  group_by(year,subsector_title,version) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE))

data1 <- data %>% 
  filter(year %in% c(1990,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_1990_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_1990_2018=rate_1990_2018*100) %>% 
  filter(year==2018)

data2 <- data %>% 
  filter(year %in% c(2010,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_2010_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_2010_2018=rate_2010_2018*100) %>% 
  filter(year==2018)

data <- left_join(data1,data2, by = c("year", "subsector_title", "GHG","version"))
aviation_summary <- rbind(aviation_summary,data)

##############

load('Data/Old data versions/edgar_data_gwp_ar6_late_2020.RData')

data <- edgar_GHG_ar6_late_2020  %>% 
  mutate(version="EDGARv5") %>% 
  filter(chapter_title=="Transport") %>% 
  group_by(year,subsector_title,version) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE))

data1 <- data %>% 
  filter(year %in% c(1990,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_1990_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_1990_2018=rate_1990_2018*100) %>% 
  filter(year==2018)

data2 <- data %>% 
  filter(year %in% c(2010,2018)) %>% 
  group_by(subsector_title) %>% 
  mutate(rate_2010_2018=((last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)) %>% 
  mutate(rate_2010_2018=rate_2010_2018*100) %>% 
  filter(year==2018)

data <- left_join(data1,data2, by = c("year", "subsector_title", "GHG","version"))
aviation_summary <- rbind(aviation_summary,data)


##############
aviation_summary <- aviation_summary %>% select(-GHG)
aviation_summary <- gather(aviation_summary,time_period,value,rate_1990_2018,rate_2010_2018)
aviation_summary <- spread(aviation_summary,version,value)

openxlsx::write.xlsx(aviation_summary,"aviation_summary.xlsx")
