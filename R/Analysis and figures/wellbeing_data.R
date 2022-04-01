
rm(list = ls())
library(tidyverse)
library(ggplot2); theme_set(theme_bw())
library(patchwork)
library(RColorBrewer)
library(openxlsx)
library(countrycode)
library(WDI)




load('Data/data_edgar_ghg.RData')



#import GDP data from worldbank
##GDP at PPP
wdi_gdp<-WDI(country = "all",indicator = "NY.GDP.MKTP.PP.KD",start = 1970,end = 2019,extra=TRUE,language = "en") %>%
  select(iso3c, year, NY.GDP.MKTP.PP.KD) %>%
  filter(!is.na(iso3c))

wdi_gdp$gdp_ppp<-wdi_gdp$NY.GDP.MKTP.PP.KD
wdi_gdp<-wdi_gdp %>% select(-NY.GDP.MKTP.PP.KD)


## Life expectancy

wdi_lex <- WDI(country = "all",indicator = "SP.DYN.LE00.IN",start = 1970,end = 2019,extra=TRUE,language = "en") %>%
  select(iso3c, year, SP.DYN.LE00.IN) %>%
  filter(!is.na(iso3c))
  
wdi_lex$life_expectancy<-wdi_lex$SP.DYN.LE00.IN
wdi_lex<-wdi_lex %>% select(-SP.DYN.LE00.IN)


## Population

wdi_pop<-WDI(country = "all",indicator = "SP.POP.TOTL",start = 1970,end = 2019,extra=TRUE,language = "en")%>%
  select(iso3c, year, SP.POP.TOTL) %>%
  filter(!is.na(iso3c))

wdi_pop$pop<-wdi_pop$SP.POP.TOTL
wdi_pop<-wdi_pop %>% select(-SP.POP.TOTL)



## join everything
data <- edgar_ghg %>% 
  group_by(ISO,country,year) %>% 
  summarise(CO2=sum(CO2,na.rm=TRUE),GHG=sum(GHG,na.rm=TRUE))

data <- left_join(data,wdi_gdp,by=c("year","ISO"="iso3c"))
data <- left_join(data,wdi_lex,by=c("year","ISO"="iso3c"))
data <- left_join(data,wdi_pop,by=c("year","ISO"="iso3c"))

data <- data %>% 
  filter(year!=2020) %>% 
  filter(!is.na(pop)) %>% 
  mutate(CO2_per_capita=CO2/pop) %>% 
  mutate(GHG_per_capita=GHG/pop) %>% 
  mutate(GDP_per_capita=gdp_ppp/pop)

write.xlsx(data,"wellbeing_data.xlsx")
