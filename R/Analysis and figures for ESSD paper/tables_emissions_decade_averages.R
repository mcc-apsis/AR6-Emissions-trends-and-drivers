rm(list = ls())
library(tidyverse)
library(lubridate)

#load('../../Data/edgar_data_gwp_ar6.RData')
#load("../../Data/edgar6_data_raw_gwp_ar5.RData")
#load('../../Data/edgar_essd_data_raw.RData')
#load('../../Data/gwps.RData')
#load("../../Data/land.RData")
#load('../../Data/edgar_essd_data_raw.RData')
load('../../Data/edgar6_v2_data_raw.RData')
load('../../Data/gwps.RData')
load("../../Data/land.RData")


# uncertainties <- data.frame(gas=c('CO2 FFI','CO2 Land use','CH4','N2O','Fgas','GHG'),
#                             uncertainty=c(0.08,0.7,0.3,0.6,0.3,0.1))
uncertainties <- data.frame(gas=c('CO2 FFI','CO2 Land use','CH4','N2O','Fgas'),
                            uncertainty=c(0.08,0.7,0.3,0.6,0.3))


wb <- openxlsx::createWorkbook(title = paste("ipcc_ar6_10_year_averages",Sys.Date()))

growth_rate <- function(years,y) {
  
  data <- data.frame(years,y)
  
  data <- data %>%
    filter(!is.na(y)) %>%
    mutate(leap_years = leap_year(years)) %>%
    mutate(y = ifelse(leap_years==TRUE,y*365/366,y))

  fit <- lm(log(y) ~ years,data = data)
  
  # data <- data %>% 
  #   mutate(rate=fit$coefficients[2]) %>% 
  #   mutate(predicted_x = exp(predict(fit,data %>% select(years)))) %>% 
  #   mutate(st_error = sqrt(diag(vcov(fit)))[2])
  # 
  # return(list("rate"=fit$coefficients[2],"data"=data))
  
  return(fit$coefficients[2])
}


GHG_data<-edgar_raw

GHG_data<-GHG_data %>%
  mutate(region_ar6_10=as.character(region_ar6_10)) %>%
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10))
  #mutate(region_ar6_10=ifelse(region_ar6_6=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  #mutate(region_ar6_10=ifelse(region_ar6_6=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10))
  
methane <- GHG_data %>% 
  filter(gas=="CH4") 

GHG_data<-GHG_data %>%
  # group_by(year,region_ar6_10,chapter_title) %>%
  # summarise_at(vars(CO2:SF6),sum,na.rm=TRUE) %>% 
  # mutate_at(vars(CO2:SF6),list(~./1e9))
  group_by(year,gas,region_ar6_10,chapter_title,gwp100_ar6) %>%   #ar6 gwps
  #group_by(year,gas,region_ar6_10,chapter_title,gwp100_ar5) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(value=value/1e9)

#GHG_data <- gather(GHG_data,gas,value,CO2:SF6)

# GHG_data <- left_join(GHG_data,gwps,by = "gas") %>%
#   select(-gwp_ar2,-gwp_ar4,-gwp_ar5,-gwp_ar6_old) %>%
#   mutate(value=value*value) %>%
#   select(-value)

GHG_data <- GHG_data %>%
  mutate(value=gwp100_ar6*value)    #ar6 gwps
  #mutate(value=gwp100_ar5*value)

#### recalculate methane in AR6 using biogenic / non-biogenic methane GWPs

# methane <- methane %>% 
#   group_by(year,region_ar6_10,chapter_title,sector_code) %>% 
#   summarise(CH4=sum(CH4,na.rm=TRUE))
# 
# methane <- left_join(methane,gwps_ch4,by = "sector_code")
# 
# methane <- methane %>% 
#   mutate(CH4_adjusted=CH4*value) %>%
#   group_by(year,region_ar6_10,chapter_title) %>%
#   summarise(CH4_adjusted=sum(CH4_adjusted)/1e9) %>% 
#   mutate(gas="CH4")

#GHG_data <- left_join(GHG_data,methane,by = c("year", "gas","region_ar6_10","chapter_title"))

# GHG_data <- GHG_data %>% 
#   mutate(value=ifelse(!is.na(CH4_adjusted),CH4_adjusted,value)) %>% 
#   select(-CH4_adjusted)

#### calculate fgas and land emissions

fgas <- GHG_data %>% 
  filter(!gas %in% c("CO2","CH4","N2O")) %>% 
  group_by(year,region_ar6_10,chapter_title) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  mutate(gas="Fgas") %>% 
  select(year,gas,everything())

land_totals <- land %>% 
  filter(year>1969) %>% 
  filter(year<2020) %>% 
  mutate(value=mean/1e9) %>%
  mutate(gas="CO2 Land use") %>% 
  mutate(chapter_title="AFOLU") %>%
  select(year,gas,region_ar6_10,chapter_title,value)

GHG_data <- GHG_data %>% 
  filter(gas %in% c("CO2","CH4","N2O"))
GHG_data <- rbind(GHG_data,fgas)
GHG_data <- rbind(GHG_data,land_totals) %>% 
  mutate(gas=ifelse(gas=="CO2","CO2 FFI",gas))




#### calculate decade averages

## by gas
# GHG_by_gas <- GHG_data %>%
#   group_by(gas,year) %>%
#   summarise(value=sum(value,na.rm=TRUE)) %>%
#   mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
#   mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
#   mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
#   mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
#   mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade)) %>%
#   group_by(gas,decade) %>%
#   summarise(value=mean(value,na.rm=TRUE))
# 
# GHG_by_gas_2019 <- GHG_data %>%
#   filter(year==2019) %>%
#   group_by(gas) %>%
#   summarise(value=sum(value,na.rm=TRUE)) %>%
#   mutate(decade="2019")
# 
# GHG_by_gas_1970 <- GHG_data %>%
#   filter(year==1970) %>%
#   group_by(gas) %>%
#   summarise(value=sum(value,na.rm=TRUE)) %>%
#   mutate(decade="1970")

GHG_by_gas <- GHG_data %>%
  group_by(gas,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  #mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade))
  mutate(decade=ifelse((year>2008 & year<=2018),"2009-2018",decade))

GHG_by_gas_2018 <- GHG_by_gas %>%
  filter(year==2018) %>%
  mutate(decade="2018")

GHG_by_gas_1970 <- GHG_by_gas %>%
  filter(year==1970) %>%
  mutate(decade="1970")

GHG_by_gas<-rbind(GHG_by_gas,GHG_by_gas_1970,GHG_by_gas_2018)

GHG <- GHG_by_gas %>%
  group_by(decade,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(gas="GHG")

GHG_by_gas <- rbind(GHG_by_gas,GHG)

GHG_by_gas <- GHG_by_gas %>%  
  group_by(gas,decade) %>%
  #mutate(avg_annual_growth=((last(value,order_by = year)/first(value,order_by = year))^(1/10)-1)*100) %>%
  mutate(avg_annual_growth=growth_rate(years = year,y=value)) %>%
  mutate(avg_annual_growth=avg_annual_growth*100) %>%
  summarise(value=mean(value,na.rm=TRUE),avg_annual_growth=first(avg_annual_growth))

GHG_by_gas <- left_join(GHG_by_gas,uncertainties,by="gas")
GHG_by_gas <- GHG_by_gas %>%
  mutate(uncertainty=uncertainty*value)

uncertainty_GHG <- GHG_by_gas %>%
  filter(gas!="GHG") %>%
  mutate(uncertainty=uncertainty^2) %>%
  group_by(decade) %>%
  summarise(uncertainty=sum(uncertainty,na.rm=TRUE)) %>%
  mutate(uncertainty=sqrt(uncertainty)) %>%
  mutate(gas="GHG") %>%
  rename(GHG_uncertainty=uncertainty)

GHG_by_gas <- left_join(GHG_by_gas,uncertainty_GHG,by=c("gas","decade")) %>%
  mutate(uncertainty=ifelse(is.na(GHG_uncertainty),uncertainty,GHG_uncertainty)) %>%
  select(-GHG_uncertainty)

GHG_growth<-GHG_by_gas %>%
  select(-value,-uncertainty) %>%
  mutate(avg_annual_growth=paste(as.character(round(avg_annual_growth,1)),"%",sep = "")) %>%
  spread(gas,avg_annual_growth) %>%
  select(decade,"CO2 FFI", "CO2 Land use", CH4, N2O, Fgas, GHG)

GHG_by_gas<-GHG_by_gas %>%
  mutate(value_uncertainty=paste(signif(value,digits=2),signif(uncertainty,digits=2),sep="±")) %>%
  select(-value,-uncertainty,-avg_annual_growth) %>%
  spread(gas,value_uncertainty) %>%
  select(decade,"CO2 FFI", "CO2 Land use", CH4, N2O, Fgas, GHG)

GHG_by_gas<-cbind(GHG_by_gas,GHG_growth)
GHG_by_gas<-GHG_by_gas[,c(1,2,9,3,10,4,11,5,12,6,13,7,14)]

GHG_by_gas<-GHG_by_gas[7:1,]

## by region

region_ar6_6<-edgar_raw%>%
  select(region_ar6_6,region_ar6_10)%>%
  unique() %>%
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_10)) %>% 
  mutate(region_ar6_10=ifelse(region_ar6_10=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_10)) %>%
  mutate(region_ar6_6=ifelse(region_ar6_6=="Intl. Aviation","Intl. Aviation and Shipping",region_ar6_6)) %>% 
  mutate(region_ar6_6=ifelse(region_ar6_6=="Intl. Shipping","Intl. Aviation and Shipping",region_ar6_6)) %>%
  unique()

GHG_by_region_ext <- left_join(GHG_data,region_ar6_6,by="region_ar6_10")

GHG_by_region <- GHG_by_region_ext %>%
  group_by(region_ar6_6,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  #mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade))
  mutate(decade=ifelse((year>2008 & year<=2018),"2009-2018",decade))

GHG_by_region_2018 <- GHG_by_region %>%
  filter(year==2018) %>%
  mutate(decade="2018")

GHG_by_region_1970 <- GHG_by_region %>%
  filter(year==1970) %>%
  mutate(decade="1970")

GHG_by_region<-rbind(GHG_by_region,GHG_by_region_1970,GHG_by_region_2018)

GHG_by_region_total <- GHG_by_region %>%
  group_by(decade, year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(region_ar6_6="world")
  
GHG_by_region<-rbind(GHG_by_region,GHG_by_region_total)

GHG_by_region <- GHG_by_region %>%  
  group_by(region_ar6_6,decade) %>%
  #mutate(avg_annual_growth=((last(value,order_by = year)/first(value,order_by = year))^(1/10)-1)*100) %>%
  mutate(avg_annual_growth=growth_rate(years = year,y=value)) %>%
  mutate(avg_annual_growth=avg_annual_growth*100) %>%
  summarise(value=mean(value,na.rm=TRUE),avg_annual_growth=first(avg_annual_growth))

GHG_growth<-GHG_by_region %>%
 select(-value) %>%
 mutate(avg_annual_growth=paste(as.character(round(avg_annual_growth,1)),"%",sep = "")) %>%
 spread(region_ar6_6,avg_annual_growth) %>%
 select("decade", "Africa", "Middle East","Asia and developing Pacific",
        "Developed Countries", "Eastern Europe and West-Central Asia",
        "Latin America and Caribbean", "Intl. Aviation and Shipping", "world")

GHG_by_region<-GHG_by_region %>%
  select(-avg_annual_growth) %>%
  mutate(value=as.character(signif(value,2))) %>%
  spread(region_ar6_6,value) %>%
  select("decade", "Africa", "Middle East","Asia and developing Pacific",
         "Developed Countries", "Eastern Europe and West-Central Asia",
         "Latin America and Caribbean", "Intl. Aviation and Shipping", "world")
  
GHG_by_region<-cbind(GHG_by_region,GHG_growth)
GHG_by_region<-GHG_by_region[,c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18)]

GHG_by_region<-GHG_by_region[7:1,]

rm(region_ar6_6)

## by sector
GHG_data_AIRSEA <- GHG_data %>%
  mutate(chapter_title=ifelse(region_ar6_10=="Intl. Aviation and Shipping","Intl. Aviation and Shipping",chapter_title))

GHG_by_sector <- GHG_data_AIRSEA %>%
  group_by(chapter_title,year) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(decade=ifelse((year>1969 & year<1980),"1970-1979",NA)) %>%
  mutate(decade=ifelse((year>1979 & year<1990),"1980-1989",decade)) %>%
  mutate(decade=ifelse((year>1989 & year<2000),"1990-1999",decade)) %>%
  mutate(decade=ifelse((year>1999 & year<2010),"2000-2009",decade)) %>%
  #mutate(decade=ifelse((year>2009 & year<2020),"2010-2019",decade))
  mutate(decade=ifelse((year>2008 & year<=2018),"2009-2018",decade))
  #group_by(chapter_title,decade) %>%
  #summarise(value=mean(value,na.rm=TRUE))

GHG_by_sector_2018 <- GHG_by_sector %>%
  filter(year==2018) %>%
  mutate(decade="2018")

GHG_by_sector_1970 <- GHG_by_sector %>%
  filter(year==1970) %>%
  mutate(decade="1970")

GHG_by_sector<-rbind(GHG_by_sector,GHG_by_sector_1970,GHG_by_sector_2018)

GHG_by_sector <- GHG_by_sector %>%  
  group_by(chapter_title,decade) %>%
  #mutate(avg_annual_growth=((last(value,order_by = year)/first(value,order_by = year))^(1/10)-1)*100) %>%
  mutate(avg_annual_growth=growth_rate(years = year,y=value)) %>%
  mutate(avg_annual_growth=avg_annual_growth*100) %>%
  summarise(value=mean(value,na.rm=TRUE),avg_annual_growth=first(avg_annual_growth))

GHG_growth<-GHG_by_sector %>%
  select(-value) %>%
  mutate(avg_annual_growth=paste(as.character(round(avg_annual_growth,1)),"%",sep = "")) %>%
  spread(chapter_title,avg_annual_growth) %>%
  select(decade, `Energy systems`,AFOLU, Industry, Transport, Buildings, 'Intl. Aviation and Shipping')

GHG_by_sector<-GHG_by_sector %>%
  select(-avg_annual_growth) %>%
  mutate(value=as.character(signif(value,2))) %>%
  spread(chapter_title,value) %>%
  select(decade, `Energy systems`,AFOLU, Industry, Transport, Buildings, 'Intl. Aviation and Shipping')

GHG_by_sector<-cbind(GHG_by_sector,GHG_growth)
GHG_by_sector<-GHG_by_sector[,c(1,2,9,3,10,4,11,5,12,6,13,7,14)]

GHG_by_sector<-GHG_by_sector[7:1,]



openxlsx::addWorksheet(wb,"Emissions by gas")
openxlsx::writeData(wb, sheet = "Emissions by gas", GHG_by_gas, colNames = T, rowNames = F)

openxlsx::addWorksheet(wb,"Emissions by region")
openxlsx::writeData(wb, sheet = "Emissions by region", GHG_by_region, colNames = T, rowNames = F)

openxlsx::addWorksheet(wb,"Emissions by sector")
openxlsx::writeData(wb, sheet = "Emissions by sector", GHG_by_sector, colNames = T, rowNames = F)

openxlsx::saveWorkbook(wb,paste0("Results/Data/ipcc_ar6_10_year_averages_gwp_ar6",".xlsx"),overwrite=T)
#openxlsx::saveWorkbook(wb,paste0("R/Analysis and figures for ESSD paper/Results/Data/ipcc_ar6_10_year_averages",".xlsx"),overwrite=T)
