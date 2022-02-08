rm(list = ls())
library(tidyverse)
library(lubridate)
source("R/Analysis and figures/growth_rate_fit.R")

load('Data/data_edgar_ghg.RData')
load('Data/data_land_co2.RData')

uncertainties <- data.frame(gas=c('CO2 FFI','CO2 Land use','CH4','N2O','Fgas'),
                            uncertainty=c(0.08,0.7,0.3,0.6,0.3))


wb <- openxlsx::createWorkbook(title = paste("ipcc_ar6_data_decadal_average_emissions"))



data<-edgar_raw

data<-data %>%
  mutate(region_ar6_10=as.character(region_ar6_10)) %>%
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10))

data<-data %>%
  group_by(year,gas,region_ar6_10,sector_title,gwp100_ar6) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(value=value/1e9)

data <- data %>%
  mutate(value=gwp100_ar6*value)

fgas <- data %>% 
  filter(!gas %in% c("CO2","CH4","N2O")) %>% 
  group_by(year,region_ar6_10,sector_title) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(gas="Fgas") %>% 
  select(year,gas,everything())

land_totals <- land %>% 
  filter(year>=1990) %>% 
  filter(year<2020) %>% 
  mutate(value=mean/1e9) %>%
  mutate(gas="CO2 Land use") %>% 
  mutate(sector_title="AFOLU") %>%
  select(year,gas,region_ar6_10,sector_title,value)

data <- data %>% 
  filter(gas %in% c("CO2","CH4","N2O"))
data <- rbind(data,fgas)
data <- rbind(data,land_totals) %>% 
  mutate(gas=ifelse(gas=="CO2","CO2 FFI",gas))

#### calculate decade averages

table <- data %>%
  group_by(gas,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",NA)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade))

table_2019 <- table %>%
  filter(year==2019) %>%
  mutate(decade="2019")

table_1990 <- table %>%
  filter(year==1990) %>%
  mutate(decade="1990")

table<-rbind(table,table_1990,table_2019)

GHG <- table %>%
  group_by(decade,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(gas="GHG")

table <- rbind(table,GHG)

table <- table %>% 
  filter(!is.na(decade))

table <- table %>%  
  group_by(gas,decade) %>%
  #mutate(avg_annual_growth=((last(value,order_by = year)/first(value,order_by = year))^(1/10)-1)*100) %>%
  mutate(avg_annual_growth=growth_rate(years = year,y=value)$rate) %>%
  mutate(avg_annual_growth=avg_annual_growth*100) %>%
  summarise(value=mean(value,na.rm=TRUE),avg_annual_growth=first(avg_annual_growth))

table <- left_join(table,uncertainties,by="gas")
table <- table %>%
  mutate(uncertainty=uncertainty*value)

uncertainty_GHG <- table %>%
  filter(gas!="GHG") %>%
  mutate(uncertainty=uncertainty^2) %>%
  group_by(decade) %>%
  summarise(uncertainty=sum(uncertainty,na.rm=TRUE)) %>%
  mutate(uncertainty=sqrt(uncertainty)) %>%
  mutate(gas="GHG") %>%
  rename(GHG_uncertainty=uncertainty)

table <- left_join(table,uncertainty_GHG,by=c("gas","decade")) %>%
  mutate(uncertainty=ifelse(is.na(GHG_uncertainty),uncertainty,GHG_uncertainty)) %>%
  select(-GHG_uncertainty)

GHG_growth<-table %>%
  select(-value,-uncertainty) %>%
  mutate(avg_annual_growth=paste(as.character(round(avg_annual_growth,1)),"%",sep = "")) %>%
  spread(gas,avg_annual_growth) %>%
  select(decade,"CO2 FFI", "CO2 Land use", CH4, N2O, Fgas, GHG)

table<-table %>%
  mutate(value_uncertainty=paste(signif(value,digits=2),signif(uncertainty,digits=2),sep="±")) %>%
  select(-value,-uncertainty,-avg_annual_growth) %>%
  spread(gas,value_uncertainty) %>%
  select(decade,"CO2 FFI", "CO2 Land use", CH4, N2O, Fgas, GHG)

table<-cbind(table,GHG_growth)
table<-table[,c(1,2,9,3,10,4,11,5,12,6,13,7,14)]

table<-table[5:1,]

openxlsx::addWorksheet(wb,"Emissions by gas")
openxlsx::writeData(wb, sheet = "Emissions by gas", table, colNames = T, rowNames = F)
openxlsx::saveWorkbook(wb,"Results/Analysis/ipcc_ar6_data_decadal_average_emissions.xlsx",overwrite=T)

