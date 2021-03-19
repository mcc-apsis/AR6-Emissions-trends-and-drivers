rm(list = ls())
library(tidyverse)

load('../../Data/edgar_data_gwp_ar6.RData')
load("../../Data/edgar_data_all.RData")
load('../../Data/gwps.RData')
load("../../Data/land.RData")

uncertainties <- data.frame(gas=c('CO2 FFI','CO2 Land use','CH4','N2O','Fgas','GHG'),
                            uncertainty=c(0.08,0.5,0.2,0.6,0.2,0.1))


wb <- openxlsx::createWorkbook(title = paste("ipcc_ar6_10_year_averages",Sys.Date()))



GHG_data<-edgar_GHG 

GHG_data<-GHG_data %>%
  mutate(region_ar6_10=as.character(region_ar6_10)) %>%
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10))
  #mutate(region_ar6_10=ifelse(region_ar6_5=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  #mutate(region_ar6_10=ifelse(region_ar6_5=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10))
  
methane <- GHG_data %>% 
  filter(!is.na(CH4)) 

GHG_data<-GHG_data %>%
  group_by(year,region_ar6_10,chapter_title) %>%
  summarise_at(vars(CO2:SF6),sum,na.rm=TRUE) %>% 
  mutate_at(vars(CO2:SF6),list(~./1e9))

GHG_data <- gather(GHG_data,gas,value,CO2:SF6)

GHG_data <- left_join(GHG_data,gwps,by = "gas") %>%
  select(-gwp_ar2,-gwp_ar4,-gwp_ar5,-gwp_ar6_old) %>%
  mutate(gwp_ar6=gwp_ar6*value) %>%
  select(-value)

#### recalculate methane in AR6 using biogenic / non-biogenic methane GWPs

methane <- methane %>% 
  group_by(year,region_ar6_10,chapter_title,sector_code) %>% 
  summarise(CH4=sum(CH4,na.rm=TRUE))

methane <- left_join(methane,gwps_ch4,by = "sector_code")

methane <- methane %>% 
  mutate(CH4_adjusted=CH4*value) %>%
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




#### calculate decade averages

## by gas
GHG_by_gas <- GHG_data %>%
  group_by(gas,year) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade)) %>%
  group_by(gas,decade) %>%
  summarise(gwp_ar6=mean(gwp_ar6,na.rm=TRUE))

GHG_by_gas_2019 <- GHG_data %>%
  filter(year==2019) %>%
  group_by(gas) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="2019")

GHG_by_gas_1970 <- GHG_data %>%
  filter(year==1970) %>%
  group_by(gas) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="1970")

GHG_by_gas<-rbind(GHG_by_gas,GHG_by_gas_1970,GHG_by_gas_2019)

GHG_by_gas_total <- GHG_by_gas %>%
  group_by(decade) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(gas="GHG")

GHG_by_gas<-rbind(GHG_by_gas,GHG_by_gas_total)

GHG_by_gas <- left_join(GHG_by_gas,uncertainties,by="gas")
GHG_by_gas <- GHG_by_gas %>%
  mutate(uncertainty=uncertainty*gwp_ar6)

GHG_by_gas<-GHG_by_gas %>%
  mutate(value_uncertainty=paste(signif(gwp_ar6,digits=2),signif(uncertainty,digits=2),sep="±")) %>%
  select(-gwp_ar6,-uncertainty) %>%
  spread(gas,value_uncertainty) %>%
  select(decade,"CO2 FFI", "CO2 Land use", CH4, N2O, Fgas, GHG)

GHG_by_gas<-GHG_by_gas[7:1,]

## by region

region_ar6_5<-edgar_GHG%>%
  select(region_ar6_5,region_ar6_10)%>%
  unique() %>%
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10)) %>%
  mutate(region_ar6_5=ifelse(region_ar6_5=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_5)) %>% 
  mutate(region_ar6_5=ifelse(region_ar6_5=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_5))

GHG_by_region_ext <- left_join(GHG_data,region_ar6_5,by="region_ar6_10")

GHG_by_region <- GHG_by_region_ext %>%
  group_by(region_ar6_5,year) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade)) %>%
  group_by(region_ar6_5,decade) %>%
  summarise(gwp_ar6=mean(gwp_ar6,na.rm=TRUE))

GHG_by_region_2019 <- GHG_by_region_ext %>%
  filter(year==2019) %>%
  group_by(region_ar6_5) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="2019")

GHG_by_region_1970 <- GHG_by_region_ext %>%
  filter(year==1970) %>%
  group_by(region_ar6_5) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="1970")

GHG_by_region<-rbind(GHG_by_region,GHG_by_region_1970,GHG_by_region_2019)

GHG_by_region_total <- GHG_by_region %>%
  group_by(decade) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(region_ar6_5="world")

GHG_by_region<-rbind(GHG_by_region,GHG_by_region_total)

GHG_by_region<-GHG_by_region %>%
  mutate(gwp_ar6=signif(gwp_ar6,2)) %>%
  spread(region_ar6_5,gwp_ar6) %>%
  select("decade", "Africa and Middle East","Asia and Developing Pacific",
         "Developed Countries", "Eastern Europe and West-Central Asia",
         "Latin America and Caribbean", "Intl. Aviation and Shipping", "world")

GHG_by_region<-GHG_by_region[7:1,]

rm(region_ar6_5)

## by sector
GHG_data_AIRSEA <- GHG_data %>%
  mutate(chapter_title=ifelse(region_ar6_10=="Intl. Aviation and Shipping","Intl. Aviation and Shipping",chapter_title))

GHG_by_sector <- GHG_data_AIRSEA %>%
  group_by(chapter_title,year) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade)) %>%
  group_by(chapter_title,decade) %>%
  summarise(gwp_ar6=mean(gwp_ar6,na.rm=TRUE))

GHG_by_sector_2019 <- GHG_data_AIRSEA %>%
  filter(year==2019) %>%
  group_by(chapter_title) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="2019")

GHG_by_sector_1970 <- GHG_data_AIRSEA %>%
  filter(year==1970) %>%
  group_by(chapter_title) %>%
  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
  mutate(decade="1970")

GHG_by_sector<-rbind(GHG_by_sector,GHG_by_sector_1970,GHG_by_sector_2019)

#GHG_by_sector_total <- GHG_by_sector %>%
#  group_by(decade) %>%
#  summarise(gwp_ar6=sum(gwp_ar6,na.rm=TRUE)) %>%
#  mutate(chapter_title="world")

#GHG_by_sector<-rbind(GHG_by_sector,GHG_by_sector_total)

GHG_by_sector<-GHG_by_sector %>%
  mutate(gwp_ar6=signif(gwp_ar6,2)) %>%
  spread(chapter_title,gwp_ar6) %>%
  select(decade, `Energy systems`,AFOLU, Industry, Transport, Buildings, 'Intl. Aviation and Shipping')

GHG_by_sector<-GHG_by_sector[7:1,]



openxlsx::addWorksheet(wb,"Emissions by gas")
openxlsx::writeData(wb, sheet = "Emissions by gas", GHG_by_gas, colNames = T, rowNames = F)

openxlsx::addWorksheet(wb,"Emissions by region")
openxlsx::writeData(wb, sheet = "Emissions by region", GHG_by_region, colNames = T, rowNames = F)

openxlsx::addWorksheet(wb,"Emissions by sector")
openxlsx::writeData(wb, sheet = "Emissions by sector", GHG_by_sector, colNames = T, rowNames = F)

openxlsx::saveWorkbook(wb,paste0("Results/Data/ipcc_ar6_10_year_averages",".xlsx"),overwrite=T)
