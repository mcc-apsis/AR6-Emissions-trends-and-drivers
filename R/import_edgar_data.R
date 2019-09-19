
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

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CO2',startRow=10)
edgar_CO2 <- edgar_CO2[1:57]
edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description,CO2 = Value)

edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CH4',startRow=10)
edgar_CH4 <- edgar_CH4[1:57]
edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description,CH4 = Value)

edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='N2O',startRow=10)
edgar_N2O <- edgar_N2O[1:58]
edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description,N2O = Value)

edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR GHG.xlsx',
                                  sheet='Fgas2',rows=10:1717)

edgar_Fgas <- edgar_Fgas[1:58]
edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2017')%>% 
  select(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description,Gas,Fgas = Value)

########### merge all Fgas into single indicator ########### 
edgar_Fgas <- edgar_Fgas %>% 
  group_by(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description) %>% 
  summarise(Fgas=sum(Fgas))

########### join sheets ########### 

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)

########### apply updated GWPs to CH4 and N2O ########### 

edgar_GHG <- edgar_GHG %>% 
  mutate(CH4 = CH4 *28/25) %>% 
  mutate(N2O = N2O *265/298)

########### calculate total GHG ########### 
edgar_GHG <- edgar_GHG %>% 
  group_by(ISO_A3,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

rm(edgar_CH4,edgar_CO2,edgar_Fgas,edgar_N2O)

########### join categories to EDGAR ########### 

load('Data/ipcc_categories.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=ISO_A3,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,description,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

########### join World Bank income classification and categories developed by Jan ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(Country,Code,WB.income),by=c("ISO"="Code"))
edgar_GHG$WB.income <- as.factor(edgar_GHG$WB.income)
edgar_GHG$WB.income <- factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])
rm(codes)

########### allocate World Bank NAs to low income ########### 

edgar_GHG <- edgar_GHG %>% 
  mutate(WB.income = ifelse(is.na(WB.income),"Low income",as.character(WB.income))) %>% 
  mutate(WB.income = as.factor(WB.income))
edgar_GHG$WB.income = factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])

save(edgar_GHG,file='Data/edgar.RData')
