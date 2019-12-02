
rm(list = ls())
library(xlsx)
library(tidyverse)

### Jos data

########### load sheets ########### 

jos_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CO2',startRow=10)
jos_CO2 <- jos_CO2[1:57]
jos_CO2 <- gather(jos_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,CO2 = Value)

jos_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:57]
jos_CH4 <- gather(jos_CH4,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,CH4 = Value)

jos_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:58]
jos_N2O <- gather(jos_N2O,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,N2O = Value)

jos_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/EDGAR GHG.xlsx',
                                  sheet='Fgas2',rows=10:1717)

jos_Fgas <- jos_Fgas[1:58]
jos_Fgas <- gather(jos_Fgas,Year,Value,'1970':'2017')%>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,Gas,Fgas = Value)

########### merge all Fgas into single indicator ########### 
jos_Fgas <- jos_Fgas %>% 
  group_by(ISO_A3,Year,EDGAR_country,IPCC.detailed,IPCC_detailed_description) %>% 
  summarise(Fgas=sum(Fgas))

########### join sheets ########### 

jos_GHG <- full_join(jos_CO2,jos_CH4)
jos_GHG <- full_join(jos_GHG,jos_N2O)
jos_GHG <- full_join(jos_GHG,jos_Fgas)

########### apply updated GWPs to CH4 and N2O ########### 

jos_GHG <- jos_GHG %>% 
  mutate(CH4 = CH4 *28/25) %>% 
  mutate(N2O = N2O *265/298)

### convert to tCO2
jos_GHG <- jos_GHG %>% 
  mutate(CO2 = CO2*1000) %>% 
  mutate(CH4 = CH4*1000) %>% 
  mutate(N2O = N2O*1000) %>% 
  mutate(Fgas = Fgas*1000)

########### calculate total GHG ########### 
jos_GHG <- jos_GHG %>% 
  group_by(ISO_A3,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

rm(jos_CH4,jos_CO2,jos_Fgas,jos_N2O)


########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_categories.RData')

jos_GHG <- left_join(jos_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter),by=c("IPCC.detailed"="code"))

jos_GHG <- jos_GHG %>% 
  select(ISO=ISO_A3,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,description,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

########### join country names and World Bank income classification ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
jos_GHG <- left_join(jos_GHG,codes %>% select(name,alpha.3,WB.income),by=c("ISO"="alpha.3"))
jos_GHG$WB.income <- as.factor(jos_GHG$WB.income)
jos_GHG$WB.income <- factor(jos_GHG$WB.income,levels(jos_GHG$WB.income)[c(1,4,3,2)])

## identify additional countries in EDGAR

missing <- jos_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO) %>% 
  unique()

jos_GHG <- jos_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)
########### allocate World Bank NAs to low income ########### 

jos_GHG <- jos_GHG %>% 
  mutate(WB.income = ifelse(is.na(WB.income),"Low income",as.character(WB.income))) %>% 
  mutate(WB.income = as.factor(WB.income))
jos_GHG$WB.income = factor(jos_GHG$WB.income,levels(jos_GHG$WB.income)[c(1,4,3,2)])



########### join country categorisation from WGIII TSU ###########

load('Data/tsu_codes.R')

jos_GHG <- left_join(jos_GHG,tsu_codes %>% select(-name),by=c("ISO"="ISO"))

missing <- jos_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev)) #%>% 
#  select(ISO,name) %>% 
#  unique()

jos_GHG <- jos_GHG %>% 
  mutate(region_ar6_5 = as.character(region_ar6_5)) %>% 
  mutate(region_ar6_10 = as.character(region_ar6_10)) %>% 
  mutate(region_ar6_22 = as.character(region_ar6_22)) %>% 
  mutate(region_ar6_dev = as.character(region_ar6_dev)) 

jos_GHG$region_ar6_5[jos_GHG$ISO=="AIR"] <- "Intl. Aviation"
jos_GHG$region_ar6_10[jos_GHG$ISO=="AIR"] <- "Intl. Aviation"
jos_GHG$region_ar6_22[jos_GHG$ISO=="AIR"] <- "Intl. Aviation"
jos_GHG$region_ar6_dev[jos_GHG$ISO=="AIR"] <- "Intl. Aviation"

jos_GHG$region_ar6_5[jos_GHG$ISO=="SEA"] <- "Intl. Shipping"
jos_GHG$region_ar6_10[jos_GHG$ISO=="SEA"] <- "Intl. Shipping"
jos_GHG$region_ar6_22[jos_GHG$ISO=="SEA"] <- "Intl. Shipping"
jos_GHG$region_ar6_dev[jos_GHG$ISO=="SEA"] <- "Intl. Shipping"


############## tidying up 

jos_GHG_old <- jos_GHG %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_10,region_ar6_22,region_ar6_dev,year=Year,chapter,sector_code,description,category_1,category_2,category_3,CO2:GHG)


save(jos_GHG_old,file='Data/edgar_old.RData')
