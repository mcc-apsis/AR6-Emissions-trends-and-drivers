
rm(list = ls())
library(xlsx)
library(tidyverse)

##### import data from matlab

# file.copy('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/MATLAB/Data shop/Aggregation/Basic data/basic.xls','Data/',overwrite=TRUE)
# 
# basic<-read.xlsx('Data/basic.xls','data_full')
# save(basic,file='Data/basic.RData')

# 
# z<- read.delim('Data/labels_T.txt',sep = '\t',header = FALSE)
# blarg <- data.frame(unique(z$V1))


### EDGAR data

########### load sheets ########### 

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CO2',startRow=10)
edgar_CO2 <- edgar_CO2[1:57]
edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,CO2 = Value)

edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CH4',startRow=10)
edgar_CH4 <- edgar_CH4[1:57]
edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,CH4 = Value)

edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='N2O',startRow=10)
edgar_N2O <- edgar_N2O[1:58]
edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,N2O = Value)

edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/dump/EDGAR GHG.xlsx',
                                  sheet='Fgas2',rows=10:1717)

edgar_Fgas <- edgar_Fgas[1:58]
edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2017')%>% 
  select(ISO_A3,Year,EDGAR_country=Name,IPCC.detailed,IPCC_detailed_description,Gas,Fgas = Value)

########### merge all Fgas into single indicator ########### 
edgar_Fgas <- edgar_Fgas %>% 
  group_by(ISO_A3,Year,EDGAR_country,IPCC.detailed,IPCC_detailed_description) %>% 
  summarise(Fgas=sum(Fgas))

########### join sheets ########### 

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)

########### apply updated GWPs to CH4 and N2O ########### 

edgar_GHG <- edgar_GHG %>% 
  mutate(CH4 = CH4 *28/25) %>% 
  mutate(N2O = N2O *265/298)

### convert to tCO2
edgar_GHG <- edgar_GHG %>% 
  mutate(CO2 = CO2*1000) %>% 
  mutate(CH4 = CH4*1000) %>% 
  mutate(N2O = N2O*1000) %>% 
  mutate(Fgas = Fgas*1000)

########### calculate total GHG ########### 
edgar_GHG <- edgar_GHG %>% 
  group_by(ISO_A3,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

rm(edgar_CH4,edgar_CO2,edgar_Fgas,edgar_N2O)


########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_categories.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=ISO_A3,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,description,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

########### join country names and World Bank income classification ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(name,alpha.3,WB.income),by=c("ISO"="alpha.3"))
edgar_GHG$WB.income <- as.factor(edgar_GHG$WB.income)
edgar_GHG$WB.income <- factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])

## identify additional countries in EDGAR

missing <- edgar_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO) %>% 
  unique()

edgar_GHG <- edgar_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)
########### allocate World Bank NAs to low income ########### 

edgar_GHG <- edgar_GHG %>% 
  mutate(WB.income = ifelse(is.na(WB.income),"Low income",as.character(WB.income))) %>% 
  mutate(WB.income = as.factor(WB.income))
edgar_GHG$WB.income = factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])



########### join country categorisation from WGIII TSU ###########

load('Data/tsu_codes.R')

edgar_GHG <- left_join(edgar_GHG,tsu_codes %>% select(-name),by=c("ISO"="ISO"))

missing <- edgar_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev)) #%>% 
#  select(ISO,name) %>% 
#  unique()

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


############## tidying up 

edgar_GHG_old <- edgar_GHG %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_10,region_ar6_22,region_ar6_dev,year=Year,chapter,sector_code,description,category_1,category_2,category_3,CO2:GHG)


save(edgar_GHG_old,file='Data/edgar_old.RData')
