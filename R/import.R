
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

rm(list = ls())
library(xlsx)
library(tidyverse)

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

edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/19-06-14-v2fin-EDGAR GHG FT2017, main tables for IPCC.XLSX',
                                 sheet='Fgas',startRow=10)
edgar_Fgas <- edgar_Fgas[1:58]
edgar_Fgas <- edgar_Fgas[1:1707,]
edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2017')%>% 
  select(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description,Gas,Fgas = Value) %>% 
  mutate(Fgas = as.numeric(Fgas))
### merge all Fgas into single indicator
edgar_Fgas <- edgar_Fgas %>% 
  group_by(ISO_A3,Year,IPCC.detailed,IPCC_detailed_description) %>% 
  summarise(Fgas=sum(Fgas))

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)


#### calculate total GHG
edgar_GHG <- edgar_GHG %>% 
  group_by(ISO_A3,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

rm(edgar_CH4,edgar_CO2,edgar_Fgas,edgar_N2O)

############# compile source categories

edgar_categories <- edgar_GHG %>% 
  filter(ISO_A3=="USA",Year==2017) %>% 
  select(code=IPCC.detailed,EDGAR_description=IPCC_detailed_description)

ipcc_ar1_categories <- read.xlsx('Data/IPCC_detailed.xlsx',sheetName='1996') %>% 
  select(IPCC.1996.Code,IPCC.1996.Name) %>% 
  filter(!is.na(IPCC.1996.Name)) %>% 
  select(code=IPCC.1996.Code,IPCC_1996_description=IPCC.1996.Name) %>% 
  mutate(source="IPCC_1996") %>% 
  mutate(IPCC_1996_description=as.character(IPCC_1996_description))

ipcc_ar5_categories <- read.xlsx('Data/IPCC Mapping.xlsx',sheetName='EmissionMap',startRow = 3,endRow = 132) %>% 
  select(code=IPCC.cat.,IPCC_AR5_sector=Sector.1,IPCC_AR5_chapter=Chapter.1,IPCC_AR5_description=IPCC_description) %>% 
  mutate(IPCC_AR5_description=as.character(IPCC_AR5_description))


master_list <- full_join(ipcc_ar1_categories,edgar_categories,by=("code"="code"))
master_list <- full_join(master_list,ipcc_ar5_categories,by=("code"="code"))

z <- master_list %>% 
  select(code,IPCC_1996_description,EDGAR_description,IPCC_AR5_description,IPCC_AR5_sector,IPCC_AR5_chapter) %>% 
arrange(code)

openxlsx::write.xlsx(z,'Data/IPCC_master_categories.xlsx',sheetName='code_comparisons',row.names = FALSE)

master_list <- master_list %>% 
  mutate(category=ifelse(is.na(EDGAR_description),IPCC_1996_description,EDGAR_description)) %>% 
  mutate(source=ifelse(is.na(source),"IPCC_AR5",source)) %>% 
  mutate(category=ifelse(is.na(category),IPCC_AR5_description,category)) %>% 
  mutate(source=ifelse(is.na(IPCC_1996_description)&is.na(IPCC_AR5_description),"EDGAR_2018",source)) %>% 
  arrange(code) %>% 
  select(code,IPCC_1996_description,IPCC_AR5_description,EDGAR_description,combined_category=category,source)

diss_list <- master_list %>% 
  select(code,combined_category,source)

category_1 <- diss_list %>% 
  filter(nchar(code)==1) %>% 
  select(code,category_1=combined_category)

category_2 <- diss_list %>% 
  filter(nchar(code)==2) %>% 
  select(code,category_2=combined_category)

category_3 <- diss_list %>% 
  filter(nchar(code)==3) %>% 
  select(code,category_3=combined_category)

category_4 <- diss_list %>% 
  filter(nchar(code)==4) %>% 
  select(code,category_4=combined_category)

category_5 <- diss_list %>% 
  filter(nchar(code)==5) %>% 
  select(code,category_5=combined_category)

category_6 <- diss_list %>% 
  filter(nchar(code)==6) %>% 
  select(code,category_6=combined_category)

diss_list <- diss_list %>% 
  mutate(handle_1 = substr(code,1,1)) %>% 
  mutate(handle_2 = ifelse(nchar(code)<2,NA,substr(code,1,2))) %>% 
  mutate(handle_3 = ifelse(nchar(code)<3,NA,substr(code,1,3))) %>% 
  mutate(handle_4 = ifelse(nchar(code)<4,NA,substr(code,1,4))) %>% 
  mutate(handle_5 = ifelse(nchar(code)<5,NA,substr(code,1,5))) %>% 
  mutate(handle_6 = ifelse(nchar(code)<6,NA,substr(code,1,6)))

diss_list <- left_join(diss_list,category_1,by=c("handle_1"="code"))
diss_list <- left_join(diss_list,category_2,by=c("handle_2"="code"))
diss_list <- left_join(diss_list,category_3,by=c("handle_3"="code"))
diss_list <- left_join(diss_list,category_4,by=c("handle_4"="code"))
diss_list <- left_join(diss_list,category_5,by=c("handle_5"="code"))
diss_list <- left_join(diss_list,category_6,by=c("handle_6"="code"))

diss_list <- diss_list %>% 
  select(-handle_1,-handle_2,-handle_3,-handle_4,-handle_5,-handle_6)

write.xlsx(master_list,'Data/IPCC_master_categories.xlsx',sheetName='full_list',row.names = FALSE,append=T)
write.xlsx(diss_list,'Data/IPCC_master_categories.xlsx',sheetName='split_list',row.names = FALSE,append=T)

#### join categories to EDGAR

edgar_GHG <- left_join(edgar_GHG,diss_list %>% select(code,combined_category,category_1,category_2,category_3),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=ISO_A3,sector_code=IPCC.detailed,category_final=combined_category,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

#### calculate total sector emissions
# totals <- gather(edgar_GHG,key,value,CO2:GHG)
# totals <- totals %>% 
#   group_by(ISO,Year,key) %>% 
#   summarise(value=sum(value,na.rm=T)) %>% 
#   mutate(sector_code="Total",category_final="Total",category_1="Total",category_2="Total",category_3="Total")
# totals <- spread(totals,key,value) %>% 
#   ungroup()
# 
# edgar_GHG <- rbind(edgar_GHG,totals)

#### join WB.codes and categories developed by Jan

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(Country,Code,WB.income),by=c("ISO"="Code"))
edgar_GHG$WB.income <- as.factor(edgar_GHG$WB.income)
edgar_GHG$WB.income <- factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])
rm(codes)

chapters <- openxlsx::read.xlsx('Data/IPCC_master_categories_JM.xlsx',sheet='code_comparisons',cols = 1:6) %>%
  select(code,IPCC_AR5_chapter,IPCC_AR5_sector)

edgar_GHG <- left_join(edgar_GHG,chapters,by=c("sector_code"="code"))

### allocate WB NAs to low income
edgar_GHG <- edgar_GHG %>% 
  mutate(WB.income = ifelse(is.na(WB.income),"Low income",as.character(WB.income))) %>% 
  mutate(WB.income = as.factor(WB.income))
edgar_GHG$WB.income = factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])

save(edgar_GHG,file='Data/edgar.RData')
