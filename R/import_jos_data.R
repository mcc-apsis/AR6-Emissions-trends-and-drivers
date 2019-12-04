
rm(list = ls())
library(xlsx)
library(tidyverse)

### Jos data

########### load CO2, CH4, N2O sheets - no GWPs applied ########### 

jos_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                 sheet='CO2',startRow=10)
jos_CO2 <- jos_CO2[1:58]
jos_CO2 <- gather(jos_CO2,Year,Value,'1970':'2018')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,Year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CO2=Value)


jos_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:58]
jos_CH4 <- gather(jos_CH4,Year,Value,'1970':'2018')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,Year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CH4=Value)


jos_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:58]
jos_N2O <- gather(jos_N2O,Year,Value,'1970':'2018')
jos_N2O <- jos_N2O %>% 
  select(ISO,Year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,N2O=Value)


########### Fgas sheets


jos_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part B--F-gases FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                  sheet='F-gas (kton)',startRow=10)
jos_Fgas <- jos_Fgas[1:58]
jos_Fgas <- gather(jos_Fgas,Year,Value,'1970':'2018')
jos_Fgas <- jos_Fgas %>% 
  select(ISO=ISO_A3,Year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,Fgas,Fgas_value=Value)

jos_Fgas <- spread(jos_Fgas,Fgas,Fgas_value)


########### join sheets ########### 

jos_GHG <- full_join(jos_CO2,jos_CH4)
jos_GHG <- full_join(jos_GHG,jos_N2O)
jos_GHG <- full_join(jos_GHG,jos_Fgas)

########### convert from Kton to t ########### 

jos_GHG <- gather(jos_GHG,key,value,CO2:SF6)
jos_GHG <- jos_GHG %>% 
  mutate(value=value*1000)
jos_GHG <- spread(jos_GHG,key,value)


rm(jos_CH4,jos_CO2,jos_Fgas,jos_N2O)


########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_categories.RData')

jos_GHG <- left_join(jos_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter),by=c("IPCC.detailed"="code"))

jos_GHG <- jos_GHG %>% 
  select(ISO,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,description,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

########### join country names ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
jos_GHG <- left_join(jos_GHG,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

## identify additional countries in EDGAR

missing_iso <- jos_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO) %>% 
  unique()

### move these countries to the name list 
jos_GHG <- jos_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)

########### join region categorisation from WGIII TSU ###########

load('Data/tsu_codes.RData')

jos_GHG <- left_join(jos_GHG,tsu_codes %>% select(-name),by=c("ISO"="ISO"))

missing_region <- jos_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev)) %>% 
  select(ISO,EDGAR_country) %>% 
  unique()


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

jos_GHG <- jos_GHG %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_10,region_ar6_22,region_ar6_dev,year=Year,chapter,sector_code,description,category_1,category_2,category_3,`c-C4F8`:SF6)

############## save 2 different files - raw and gwp_ar5

load('Data/gwps.RData')

jos_GHG <- jos_GHG %>% filter(year>=1979)

jos_GHG_gwp_ar5 <- gather(jos_GHG,key,value,`c-C4F8`:SF6)
jos_GHG_gwp_ar5 <- left_join(jos_GHG_gwp_ar5,gwps %>% select(Fgas,gwp_ar5),by=c("key"="Fgas"))
jos_GHG_gwp_ar5 <- jos_GHG_gwp_ar5 %>% 
  mutate(value=value*gwp_ar5)
jos_GHG_gwp_ar5 <- spread(jos_GHG_gwp_ar5,key,value)

#jos_GHG_gwp_ar5 < jos_GHG_gwp_ar5 %>% 
#  mutate(GHG=sum())

save(jos_GHG,file='Data/edgar_old.RData')
