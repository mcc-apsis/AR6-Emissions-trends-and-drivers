

rm(list = ls())
library(tidyverse)

load('Data/edgar5_data_raw.RData')
edgar5_raw <- edgar_raw
load('Data/edgar6_data_raw.RData')
edgar6_raw <- edgar_raw
rm(edgar_raw)

edgar5_ch4 <- edgar5_raw %>% 
  filter(gas=="CH4") %>% 
  group_by(year) %>% 
  summarise(edgar5_ch4=sum(value))

edgar6_ch4 <- edgar6_raw %>% 
  filter(gas=="CH4") %>% 
  group_by(year) %>% 
  summarise(edgar6_ch4=sum(value))

ch4 <- left_join(edgar5_ch4,edgar6_ch4,by="year")

ch4 <- ch4 %>% 
  mutate(abs_difference=edgar6_ch4-edgar5_ch4) %>% 
  mutate(rel_difference=(1-(edgar6_ch4/edgar5_ch4))*100)

openxlsx::write.xlsx(ch4,"ch4.xlsx")
