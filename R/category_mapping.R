
rm(list = ls())
library(xlsx)
library(tidyverse)

### EDGAR data

########### load sheets ########### 

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='CO2_FT2018',startRow=5)
names(edgar_CO2) <- gsub(x = names(edgar_CO2), pattern = "Y_", replacement = "")  

edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CO2 = Value)



edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='CH4',startRow=5)
names(edgar_CH4) <- gsub(x = names(edgar_CH4), pattern = "Y_", replacement = "")  

edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CH4 = Value)



edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='N2O',startRow=5)
names(edgar_N2O) <- gsub(x = names(edgar_N2O), pattern = "Y_", replacement = "")  

edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,N2O = Value)



edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                  sheet='GWP_100_AR5_Fgases',startRow=5)
names(edgar_Fgas) <- gsub(x = names(edgar_Fgas), pattern = "Y_", replacement = "")  

edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1990':'2015')%>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,Fgas = Value)


########### join sheets ########### 

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)

rm(edgar_CO2,edgar_CH4,edgar_Fgas,edgar_N2O)

########### compile source categories ########### 
########### we use 3: the edgar database, IPCC AR5 and IPCC AR2 ########### 

edgar_categories <- edgar_GHG %>% 
  select(code=IPCC.detailed,EDGAR_description=IPCC_detailed_description) %>% 
  distinct(code,.keep_all = TRUE)

edgar_categories <- edgar_categories %>% 
  arrange(code)

ipcc_ar2_categories <- read.xlsx('Data/IPCC_AR4_2_sector_mapping.xlsx',sheetName='1996') %>% 
  select(IPCC.1996.Code,IPCC.1996.Name) %>% 
  filter(!is.na(IPCC.1996.Name)) %>% 
  select(code=IPCC.1996.Code,IPCC_AR2_description=IPCC.1996.Name) %>% 
  mutate(source="IPCC_1996") %>% 
  mutate(IPCC_AR2_description=as.character(IPCC_AR2_description))

ipcc_ar5_categories <- read.xlsx('Data/IPCC_AR5_sector_mapping.xlsx',sheetName='EmissionMap',startRow = 3,endRow = 132) %>% 
  select(code=IPCC.cat.,IPCC_AR5_sector=Sector.1,IPCC_AR5_chapter=Chapter.1,IPCC_AR5_description=IPCC_description) %>% 
  mutate(IPCC_AR5_description=as.character(IPCC_AR5_description))

master_list <- full_join(ipcc_ar2_categories,edgar_categories,by=("code"="code"))
master_list <- full_join(master_list,ipcc_ar5_categories,by=("code"="code"))

### join the editable list with chapter allocations for AR6

ipcc_ar6_chapters <- read.xlsx('Data/IPCC_categories_and_chapters_EDITABLE.xlsx',sheetName = 'code_comparisons') %>% 
  select(code,IPCC_AR6_chapter)

master_list <- full_join(master_list,ipcc_ar6_chapters,by=("code"="code")) %>% 
  select(code,IPCC_AR6_chapter,everything()) %>% 
  arrange(code)

file.remove('Results/IPCC_master_categories.xlsx')
openxlsx::write.xlsx(master_list %>% select(-source),'Results/IPCC_master_categories.xlsx',sheetName='code_comparisons',row.names = FALSE)

master_list <- master_list %>% 
  mutate(category=ifelse(is.na(EDGAR_description),IPCC_AR2_description,EDGAR_description)) %>% 
  mutate(source=ifelse(is.na(source),"IPCC_AR5",source)) %>% 
  mutate(category=ifelse(is.na(category),IPCC_AR5_description,category)) %>% 
  mutate(source=ifelse(is.na(IPCC_AR2_description)&is.na(IPCC_AR5_description),"EDGAR_2018",source)) %>% 
  arrange(code) %>% 
  select(code,IPCC_AR6_chapter,IPCC_AR2_description,IPCC_AR5_description,EDGAR_description,combined_category=category,source)

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

ipcc_categories <- left_join(ipcc_categories,master_list %>% select(code,IPCC_AR6_chapter),by=("code"="code")) %>%
  select(code,IPCC_AR6_chapter,description=combined_category,description_source=source,everything()) %>% 
  distinct(code,.keep_all=TRUE) %>% 
  arrange(IPCC_AR6_chapter)

write.xlsx(ipcc_categories,'Results/IPCC_master_categories.xlsx',sheetName='chapter_list',row.names = FALSE,append=T)

save(ipcc_categories,file='Data/ipcc_categories.RData')
