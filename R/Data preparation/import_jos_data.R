
rm(list = ls())
library(tidyverse)

### Jos data

########### load CO2, CH4, N2O sheets - no GWPs applied ########### 

jos_CO2 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                 sheet='CO2',startRow=10)
jos_CO2 <- jos_CO2[1:58]
jos_CO2 <- gather(jos_CO2,year,value,'1970':'2018')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CO2=value)


jos_CH4 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:58]
jos_CH4 <- gather(jos_CH4,year,value,'1970':'2018')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CH4=value)


jos_N2O <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:58]
jos_N2O <- gather(jos_N2O,year,value,'1970':'2018')
jos_N2O <- jos_N2O %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,N2O=value)


########### Fgas sheets


jos_Fgas <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part B--F-gases FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                  sheet='F-gas (kton)',startRow=10)
jos_Fgas <- jos_Fgas[1:58]
jos_Fgas <- gather(jos_Fgas,year,value,'1970':'2018')
jos_Fgas <- jos_Fgas %>% 
  select(ISO=ISO_A3,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,Fgas,Fgas_value=value)

jos_Fgas <- spread(jos_Fgas,Fgas,Fgas_value)


########### join sheets ########### 

edgar_GHG <- full_join(jos_CO2,jos_CH4)
edgar_GHG <- full_join(edgar_GHG,jos_N2O)
edgar_GHG <- full_join(edgar_GHG,jos_Fgas)

########### convert from Kton to t ########### 

load('Data/gwps.RData')
for (row in 1:nrow(gwps)) {
  col <- gwps[row, "gas"]
  edgar_GHG[col] <- edgar_GHG[col]*1000
}

# edgar_GHG <- gather(edgar_GHG,key,value,CO2:SF6)
# edgar_GHG <- edgar_GHG %>% 
#   mutate(value=value*1000)
# edgar_GHG <- spread(edgar_GHG,key,value)

rm(jos_CH4,jos_CO2,jos_Fgas,jos_N2O)


########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_categories.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter,IPCC_AR6_chapter_title),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,chapter_title=IPCC_AR6_chapter_title,description,category_1,category_2,category_3,year,everything(),-IPCC_detailed_description)

missing_codes <- edgar_GHG %>% 
  filter(is.na(chapter) | is.na(description) | is.na(sector_code)) %>% 
  unique()


########### join country names ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

## identify additional countries in EDGAR

missing_iso <- edgar_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO) %>% 
  unique()

### move these countries to the name list 
edgar_GHG <- edgar_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)

########### join region categorisation from WGIII TSU ###########

load('Data/tsu_codes.RData')

edgar_GHG <- left_join(edgar_GHG,tsu_codes %>% select(-name),by=c("ISO"="ISO"))

missing_region <- edgar_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_5_short)) %>%
  select(ISO,EDGAR_country) %>% 
  unique()


edgar_GHG <- edgar_GHG %>% 
  mutate(region_ar6_5 = as.character(region_ar6_5)) %>% 
  mutate(region_ar6_10 = as.character(region_ar6_10)) %>% 
  mutate(region_ar6_22 = as.character(region_ar6_22)) %>% 
  mutate(region_ar6_dev = as.character(region_ar6_dev))

edgar_GHG$region_ar6_5[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"

edgar_GHG$region_ar6_5[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"

edgar_GHG$region_ar6_5_short[edgar_GHG$ISO=="SEA"] <- "SEA"
edgar_GHG$region_ar6_5_short[edgar_GHG$ISO=="AIR"] <- "AIR"


############## tidying up 

edgar_GHG <- edgar_GHG %>% 
  mutate(year=as.numeric(year)) %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_5_short,region_ar6_10,region_ar6_22,region_ar6_dev,year,chapter,chapter_title,sector_code,description,category_1,category_2,category_3,CO2,CH4,N2O,everything(),-EDGAR_country)


############## factorise regions


edgar_GHG_ar5$region_ar6_5_short <- as.factor(edgar_GHG_ar5$region_ar6_5_short)
edgar_GHG_ar5$region_ar6_5_short <- factor(edgar_GHG_ar5$region_ar6_5_short,levels(edgar_GHG_ar5$region_ar6_5_short)[c(1,7,2,3,4,5,6)])


############## calculate gwps based on ar5 values

edgar_GHG_ar5 <- edgar_GHG
for (row in 1:nrow(gwps)) {
  col <- gwps[row, "gas"]
  gwp  <- gwps[row, "gwp_ar5"]
  edgar_GHG_ar5[col] <- edgar_GHG_ar5[col]*gwp
}

fgas_list <- gwps %>% 
  filter(gas!="CO2") %>% 
  filter(gas!="N2O") %>% 
  filter(gas!="CH4") %>% 
  select(gas)

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  mutate(Fgas=rowSums(.[fgas_list$gas],na.rm=T)) %>% 
  mutate(nnums=rowSums(!is.na(.[fgas_list$gas]))) %>% 
  mutate(Fgas=ifelse(nnums==0,NA,Fgas)) %>% 
  select(-nnums,-one_of(fgas_list$gas))

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  group_by(ISO,year,sector_code) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))


############## save two datasets

save(edgar_GHG,file='Data/edgar_data_all.RData')
save(edgar_GHG_ar5,file='Data/edgar_data_gwp_ar5.RData')
