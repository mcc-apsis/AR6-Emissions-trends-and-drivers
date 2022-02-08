
#### to estimate uncertainties when GWP metric uncertainty is included 

rm(list = ls())
library(tidyverse)
library(openxlsx)

load('Data/data_edgar_ghg.RData')
load('Data/gwps.RData')
load("Data/land.RData")

land <- land %>% 
  filter(year>1969) %>%
  group_by(year) %>% 
  summarise(value=sum(mean,na.rm=TRUE)) %>% 
  mutate(gas="CO2 Land use")


uncertainties <- data.frame(gas=c('CO2 FFI','CO2 Land use','CH4','N2O','Fgas','GHG'),
                            uncertainty_gas=c(0.08,0.7,0.3,0.6,0.3,0.1))


gases <- edgar_raw %>%  
  group_by(year,gas,gwp100_ar6) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(gas=as.character(gas)) %>% 
  mutate(gas=ifelse(gas=="CO2","CO2 FFI",gas))

gases <- rbind(gases,land)
gases <- gases %>% 
  mutate(gwp100_ar6=ifelse(gas=="CO2 Land use",1,gwp100_ar6)) %>% 
  mutate(value_gwp=value*gwp100_ar6)

gases <- left_join(gases,uncertainties,by="gas")

gases <- gases %>% 
  mutate(uncertainty_gas=ifelse(is.na(uncertainty_gas),0.3,uncertainty_gas))

gases <- left_join(gases,gwps %>% select(gas,lifetime_ar6),by="gas")

gases <- gases %>% 
  mutate(uncertainty_gwp=ifelse(lifetime_ar6<20,0.5,0.4)) %>% 
  mutate(uncertainty_gwp=ifelse(gas=="CO2 FFI",0,uncertainty_gwp)) %>% 
  mutate(uncertainty_gwp=ifelse(gas=="CO2 Land use",0,uncertainty_gwp))

gases <- gases %>% 
  mutate(total_uncertainty=sqrt(uncertainty_gas^2 + uncertainty_gwp^2)) %>% 
  mutate(`+/-`=total_uncertainty*value_gwp) %>% 
  filter(year==2019) %>% 
  arrange(desc(value_gwp))


write.xlsx(gases,"Results/Data/aggregate_uncertainty.xlsx")

## then sum the aggregate uncertainties with and without metric uncertainty in excel, and calculate as a fraction of total emissions

