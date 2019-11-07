
rm(list = ls())
library(xlsx)
library(tidyverse)

### EDGAR data

########### load sheets ########### 

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR_v5.0_FT2018_CO2_GHG_AR4.xlsx',
                                 sheet='CO2_FT2018',startRow=6)
names(edgar_CO2) <- gsub(x = names(edgar_CO2), pattern = "Y_", replacement = "")  

edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2017') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CO2 = Value)



edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR_v5.0_FT2018_CO2_GHG_AR4.xlsx',
                                 sheet='GWP_100_AR4_CH4',startRow=5)
names(edgar_CH4) <- gsub(x = names(edgar_CH4), pattern = "Y_", replacement = "")  

edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CH4 = Value)



edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR_v5.0_FT2018_CO2_GHG_AR4.xlsx',
                                 sheet='GWP_100_AR4_N2O',startRow=5)
names(edgar_N2O) <- gsub(x = names(edgar_N2O), pattern = "Y_", replacement = "")  

edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,N2O = Value)



edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR_v5.0_FT2018_CO2_GHG_AR4.xlsx',
                                  sheet='GWP_100_AR4_F-gases',startRow=5)
names(edgar_Fgas) <- gsub(x = names(edgar_Fgas), pattern = "Y_", replacement = "")  

edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2015')%>% 
  select(ISO_A3=Country_code_A3,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,Fgas = Value)


########### join sheets ########### 

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)

########### apply updated GWPs to CH4 and N2O ########### 

# edgar_GHG <- edgar_GHG %>% 
#   mutate(CH4 = CH4 *28/25) %>% 
#   mutate(N2O = N2O *265/298)

# to get 
# 1/GWP


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


########### calculate total sector emissions ########### 

totals <- edgar_GHG %>% 
  group_by(ISO,Year,WB.income,Country) %>% 
  summarise(CO2=sum(CO2,rm.na=T),CH4=sum(CH4,rm.na=T),N2O=sum(N2O,rm.na=T),Fgas=sum(Fgas,rm.na=T),GHG=sum(GHG,rm.na=T)) %>% 
  mutate(sector_code="Total",category_final="Total",category_1="Total",category_2="Total",category_3="Total",IPCC_AR5_chapter=0,IPCC_AR5_sector=0) %>% 
  select(ISO,sector_code,category_final,category_1,category_2,category_3,Year,CO2,CH4,N2O,Fgas,GHG,Country,WB.income,IPCC_AR5_chapter,IPCC_AR5_sector)

edgar_GHG <- edgar_GHG %>% 
  bind_rows(totals)

############## tidying up 

edgar_GHG <- edgar_GHG %>% 
  select(ISO,country=Country,year=Year,region_wb=WB.income,chapter,sector_code,description,category_1,category_2,category_3,CO2:GHG)

save(edgar_GHG,file='Data/edgar.RData')
