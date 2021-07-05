
################ for Jan's paper


load('../../Data/edgar_essd_data_ghg_gwp_ar5.RData')


sectors <- edgar_ghg %>% 
  group_by(year,subsector_title) %>% 
  summarise(value=sum(GHG,na.rm=TRUE)/1e9)

sectors <- sectors %>% 
  mutate(cut=ifelse(year<1980,'1970-1979',NA)) %>% 
  mutate(cut=ifelse(year>2009,'2010-2019',cut))

sectors <- sectors %>% 
  filter(!is.na(cut))

sectors <- sectors %>% 
  group_by(subsector_title,cut) %>% 
  summarise(value=mean(value))

sectors <- sectors %>% 
  group_by(subsector_title) %>% 
  mutate(abs_difference=last(value)-first(value)) %>% 
  mutate(rel_difference=last(value/first(value)))

write.xlsx(sectors,"subsector_trends.xlsx")



################## for the Washington Post

rm(list = ls())
library(tidyverse); options(dplyr.summarise.inform = FALSE)

load('Data/Old data versions/edgar_data_gwp_ar6_late_2020.RData')


subsectors <- edgar_GHG_ar6_late_2020  %>%
  filter(year>1969) %>%
  filter(year<2019) %>%
  group_by(year,chapter_title,subsector_title) %>%
  summarise_at(vars(CO2,CH4,N2O,Fgas,GHG),sum,na.rm=TRUE) %>% 
  mutate_at(vars(CO2,CH4,N2O,Fgas,GHG),~./1e9) %>% 
#  summarise(total_GHG=sum(GHG,na.rm=TRUE)/1e9) %>%
  ungroup()


########## add land CO2 data
load('Data/land.RData')
land <- land %>%
  filter(year>1969) %>% 
  filter(year<2019) %>% 
  mutate(chapter_title="AFOLU") %>%
  mutate(subsector_title="Land-use (CO2)") %>% 
  group_by(year,chapter_title,subsector_title) %>%
  summarise(CO2=sum(mean,na.rm=TRUE)/1e9) %>% 
  mutate(CH4=0) %>% 
  mutate(N2O=0) %>% 
  mutate(Fgas=0) %>% 
  mutate(GHG=CO2)


subsectors <- rbind(subsectors,land)


########### add indirect emissions
load("Data/Old data versions/indirect_CO2.RData")
indirect_CO2 <- indirect_CO2_regions %>% 
  group_by(year,chapter_title,subsector_title) %>% 
  summarise(CO2_indirect=sum(CO2_indirect)) %>% 
  ungroup()



subsectors <- left_join(subsectors,indirect_CO2,by = c("year","chapter_title","subsector_title"))

subsectors <- subsectors %>% 
  arrange(year,chapter_title,subsector_title)


subsectors <- subsectors %>% 
  mutate(GHG_plus_indirect = GHG+CO2_indirect) %>% 
  mutate(GHG_plus_indirect = ifelse(is.na(GHG_plus_indirect),GHG,GHG_plus_indirect)) 


subsectors <- subsectors %>% 
  mutate(subsector_title=ifelse(subsector_title=="Fuel combustion (REMOVE FROM FIGURES)","Fuel combustion",subsector_title))


openxlsx::write.xlsx(subsectors,"subsector_emissions.xlsx")
