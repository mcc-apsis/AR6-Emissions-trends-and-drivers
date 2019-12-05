
rm(list = ls())
library(openxlsx)
library(tidyverse)

### EDGAR data

########### load sheets ########### 


jos_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='CO2',startRow=10)
jos_CO2 <- jos_CO2[1:58]
jos_CO2 <- gather(jos_CO2,year,value,'1970':'2018')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CO2=value)


jos_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:58]
jos_CH4 <- gather(jos_CH4,year,value,'1970':'2018')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CH4=value)


jos_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:58]
jos_N2O <- gather(jos_N2O,year,value,'1970':'2018')
jos_N2O <- jos_N2O %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,N2O=value)


########### Fgas sheets


jos_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part B--F-gases FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
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

########### compile source categories ########### 
########### we use 3: the edgar database, IPCC AR5 and IPCC AR2 ########### 

edgar_categories <- edgar_GHG %>% 
  select(code=IPCC.detailed,EDGAR_description=IPCC_detailed_description) %>% 
  distinct(code,.keep_all = TRUE)

edgar_categories <- edgar_categories %>% 
  arrange(code)

ipcc_ar2_categories <- read.xlsx('Data/IPCC_AR4_2_sector_mapping.xlsx',sheet='1996') %>% 
  select(IPCC.1996.Code,IPCC.1996.Name) %>% 
  filter(!is.na(IPCC.1996.Name)) %>% 
  select(code=IPCC.1996.Code,IPCC_AR2_description=IPCC.1996.Name) %>% 
  mutate(source="IPCC_1996") %>% 
  mutate(IPCC_AR2_description=as.character(IPCC_AR2_description))

ipcc_ar5_categories <- read.xlsx('Data/IPCC_AR5_sector_mapping.xlsx',sheet='EmissionMap',startRow = 3) %>% 
  select(code=IPCC.cat.,IPCC_AR5_sector=Sector_A,IPCC_AR5_chapter=Chapter_a,IPCC_AR5_description=IPCC_description) %>% 
  mutate(IPCC_AR5_description=as.character(IPCC_AR5_description))

master_list <- full_join(ipcc_ar2_categories,edgar_categories,by=("code"="code"))
master_list <- full_join(master_list,ipcc_ar5_categories,by=("code"="code"))

### join the editable list with chapter allocations for AR6

ipcc_ar6_chapters <- read.xlsx('Data/IPCC_categories_and_chapters_EDITABLE.xlsx',sheet = 'code_comparisons') %>% 
  select(code,IPCC_AR6_chapter)

master_list <- full_join(master_list,ipcc_ar6_chapters,by=("code"="code")) %>% 
  select(code,IPCC_AR6_chapter,everything()) %>% 
  arrange(code)

######## join with shorter sector/chapter names ######## 

sectors = data.frame(key=c(6,7,9,10,11),IPCC_AR6_chapter_title=c("Energy systems","AFOLU","Buildings","Transport","Industry"))
master_list <- left_join(master_list,sectors,by=c("IPCC_AR6_chapter"="key"))

master_list <- master_list %>% 
  mutate(category=ifelse(is.na(EDGAR_description),IPCC_AR2_description,EDGAR_description)) %>% 
  mutate(source=ifelse(is.na(source),"IPCC_AR5",source)) %>% 
  mutate(category=ifelse(is.na(category),IPCC_AR5_description,category)) %>% 
  mutate(source=ifelse(is.na(IPCC_AR2_description)&is.na(IPCC_AR5_description),"EDGAR_2018",source)) %>% 
  arrange(code) %>% 
  select(code,IPCC_AR6_chapter,IPCC_AR6_chapter_title,IPCC_AR2_description,IPCC_AR5_description,EDGAR_description,combined_category=category,source)


########## write it

wb <- createWorkbook(title = "IPCC_master_categories")
addWorksheet(wb,"master_list")
writeData(wb, sheet = "master_list", master_list, rowNames = F)

########## 


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

ipcc_categories <- diss_list %>% 
  select(-handle_1,-handle_2,-handle_3,-handle_4,-handle_5,-handle_6)

ipcc_categories <- ipcc_categories %>% 
  distinct(code,.keep_all = TRUE)

ipcc_categories <- left_join(ipcc_categories,master_list %>% select(code,IPCC_AR6_chapter,IPCC_AR6_chapter_title),by=("code"="code")) %>%
  select(code,IPCC_AR6_chapter,IPCC_AR6_chapter_title,description=combined_category,description_source=source,everything()) %>% 
  distinct(code,.keep_all=TRUE) %>% 
  arrange(IPCC_AR6_chapter)

addWorksheet(wb,"chapter_list")
writeData(wb, sheet = "chapter_list", ipcc_categories, rowNames = F)
saveWorkbook(wb,"Results/IPCC_master_categories.xlsx",overwrite = T)

########## AR5 vs AR6

ipcc_ar5_sectors <- read.xlsx('Data/IPCC_AR5_sector_mapping.xlsx',sheet='Sectors')

ipcc_ar5_categories <- left_join(ipcc_ar5_categories,ipcc_ar5_sectors,by=c("IPCC_AR5_sector"="code")) %>% 
  select(code,IPCC_AR5_chapter,IPCC_AR5_sector=sector,IPCC_AR5_description)


ipcc_ar5_categories <- full_join(ipcc_ar5_categories,edgar_categories)

ipcc_ar5_categories <- left_join(ipcc_ar5_categories,master_list %>% select(code,IPCC_AR6_chapter,IPCC_AR6_chapter_title))

ipcc_ar5_categories <- ipcc_ar5_categories %>% 
  mutate(IPCC_AR6_chapter_title = as.character(IPCC_AR6_chapter_title)) %>% 
  mutate(IPCC_AR6_chapter=ifelse(is.na(EDGAR_description),NA,IPCC_AR6_chapter)) %>% 
  mutate(IPCC_AR6_chapter_title=ifelse(is.na(EDGAR_description),NA,IPCC_AR6_chapter_title)) %>% 
  mutate(description=EDGAR_description) %>% 
  mutate(description=ifelse(is.na(description),IPCC_AR5_description,description))
  

ipcc_ar5_categories <- ipcc_ar5_categories %>% 
  select(code,description,IPCC_AR6_chapter,IPCC_AR6_chapter_title,IPCC_AR5_chapter,IPCC_AR5_sector) %>% 
  arrange(IPCC_AR6_chapter)

rm(edgar_GHG)
load('Data/edgar_data_gwp_ar5.RData')
data <- edgar_GHG_ar5 %>%
  filter(year==2018) %>% 
  group_by(sector_code) %>% 
  summarise(GHG=sum(GHG,na.rm=T))

total <- sum(data$GHG)

data <- data %>% 
  mutate(fraction_global=GHG/total)

ipcc_ar5_categories <- left_join(ipcc_ar5_categories,data,by=c("code"="sector_code"))

ipcc_ar5_categories <- ipcc_ar5_categories %>% 
  select(code,description,IPCC_AR6_chapter,IPCC_AR6_chapter_title,GHG_2018=GHG,GHG_fraction_2018=fraction_global,IPCC_AR5_chapter,IPCC_AR5_sector) %>% 
  arrange(IPCC_AR6_chapter)


########## write it




wb2 <- createWorkbook(title = "edgar_ar5_ar6_code_comparison")
addWorksheet(wb2,"comparison")
writeData(wb2, sheet = "comparison", ipcc_ar5_categories, rowNames = F)
saveWorkbook(wb2,"Results/edgar_ar5_ar6_code_comparison.xlsx",overwrite = T)














save(ipcc_categories,file='Data/ipcc_categories.RData')
