
rm(list = ls())
library(xlsx)
library(tidyverse)

##### import data from matlab

file.copy('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/MATLAB/Data shop/Aggregation/Basic data/basic.xls','Data/',overwrite=TRUE)

basic<-read.xlsx('Data/basic.xls','data_full')
save(basic,file='Data/basic.RData')

# 
# z<- read.delim('Data/labels_T.txt',sep = '\t',header = FALSE)
# blarg <- data.frame(unique(z$V1))


### EDGAR data

rm(list = ls())
library(tidyverse)

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CO2',startRow=10)
edgar_CO2 <- edgar_CO2[1:57]
edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,CO2 = Value)

edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='CH4',startRow=10)
edgar_CH4 <- edgar_CH4[1:57]
edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,CH4 = Value)


edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='N2O',startRow=10)
edgar_N2O <- edgar_N2O[1:58]
edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2017') %>% 
  select(ISO_A3,Year,IPCC.detailed,N2O = Value)

edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='Fgas',startRow=10)
edgar_Fgas <- edgar_Fgas[1:58]
edgar_Fgas <- edgar_Fgas[1:1707,]
edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2017')%>% 
  select(ISO_A3,Year,IPCC.detailed,Fgas = Value) %>% 
  mutate(Fgas = as.numeric(Fgas))


edgar_GHG <- left_join(edgar_CO2,edgar_CH4)
edgar_GHG <- left_join(edgar_GHG,edgar_N2O)
edgar_GHG <- left_join(edgar_GHG,edgar_Fgas)


#### calculate total GHG
edgar_GHG <- edgar_GHG %>% 
  group_by(ISO_A3,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))


#### join source categories
categories <- read.xlsx('Data/IPCC Mapping.xlsx',sheetName='EmissionMap',startRow=3)
sectors <- read.xlsx('Data/IPCC Mapping.xlsx',sheetName='Sectors')

categories <- left_join(categories,sectors,by=c("Sector"="code"))
categories <- left_join(categories,sectors,by=c("Chapter"="code")) %>% 
  select(IPCC.detailed=IPCC.cat.,IPCC.description=IPCC_description,Sector=sector.x,Chapter=sector.y)

edgar_GHG <- left_join(edgar_GHG,categories,by=c("IPCC.detailed"="IPCC.detailed"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=ISO_A3,IPCC.detailed,IPCC.description,Sector,Chapter,Year,everything())


#### calculate total sector emissions
totals <- gather(edgar_GHG,key,value,CO2:GHG)
totals <- totals %>% 
  group_by(ISO,Year,key) %>% 
  summarise(value=sum(value,na.rm=T)) %>% 
  mutate(Chapter="Total",Sector="Total",IPCC.detailed="Total",IPCC.description="Total")
totals <- spread(totals,key,value) %>% 
  select(ISO,IPCC.detailed,IPCC.description,Sector,Chapter,Year,CO2,CH4,N2O,Fgas,GHG) %>% 
  ungroup()

edgar_GHG <- rbind(edgar_GHG,totals)

save(edgar_GHG,file='Data/edgar.RData')
