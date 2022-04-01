rm(list = ls())
library(tidyverse)
library(WDI)

#import GDP data from worldbank
##GDP at PPP
wdi_gdp<-WDI(country = "all",indicator = "NY.GDP.MKTP.PP.KD",start = 1970,end = 2019,extra=TRUE,language = "en") %>%
  select(iso3c, country, year, NY.GDP.MKTP.PP.KD) %>%
  filter(!is.na(iso3c))

wdi_gdp$gdp_ppp<-wdi_gdp$NY.GDP.MKTP.PP.KD
wdi_gdp<-wdi_gdp %>% select(-NY.GDP.MKTP.PP.KD)

##GDP in US$
wdi_gdp_usd<-WDI(country = "all",indicator = "NY.GDP.MKTP.KD",start = 1970,end = 2019,extra=TRUE,language = "en") %>%
  select(iso3c, country, year, NY.GDP.MKTP.KD) %>%
  filter(!is.na(iso3c))
  
wdi_gdp_usd$gdp_real<-wdi_gdp_usd$NY.GDP.MKTP.KD
wdi_gdp_usd<-wdi_gdp_usd %>% select(-NY.GDP.MKTP.KD)

#import population data from worldbank
wdi_pop<-WDI(country = "all",indicator = "SP.POP.TOTL",start = 1970,end = 2019,extra=TRUE,language = "en")%>%
  select(iso3c, country, year, SP.POP.TOTL) %>%
  filter(!is.na(iso3c))

wdi_pop$population<-wdi_pop$SP.POP.TOTL
wdi_pop<-wdi_pop %>% select(-SP.POP.TOTL)


#elec access
wdi_elec<-WDI(country = "all",indicator = "EG.ELC.ACCS.ZS",start = 1970,end = 2019,extra=TRUE,language = "en")%>%
  select(iso3c, country, year, EG.ELC.ACCS.ZS) %>%
  filter(!is.na(iso3c))

wdi_elec$elec_access<-wdi_elec$EG.ELC.ACCS.ZS
wdi_elec<-wdi_elec %>% select(-EG.ELC.ACCS.ZS)

#
wdi_cooking<-WDI(country = "all",indicator = "EG.CFT.ACCS.ZS",start = 1970,end = 2019,extra=TRUE,language = "en")%>%
  select(iso3c, country, year, EG.CFT.ACCS.ZS) %>%
  filter(!is.na(iso3c))

wdi_cooking$clean_cooking<-wdi_cooking$EG.CFT.ACCS.ZS
wdi_cooking<-wdi_cooking %>% select(-EG.CFT.ACCS.ZS)


#join GDP and population data
wdi_data_gdp_pop <- right_join(wdi_gdp,wdi_pop, by=c("iso3c","year","country"))
wdi_data_gdp_pop <- right_join(wdi_data_gdp_pop,wdi_gdp_usd, by=c("iso3c","year","country"))
wdi_data_gdp_pop <- left_join(wdi_data_gdp_pop,wdi_elec, by=c("iso3c","year","country"))
wdi_data_gdp_pop <- left_join(wdi_data_gdp_pop,wdi_cooking, by=c("iso3c","year","country"))

#save data
save(wdi_data_gdp_pop,file="Data/data_WDI_gdp_pop.RData")
