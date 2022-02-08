
rm(list = ls())
library(tidyverse)

# load('Data/edgar_data_all.RData')
# edgar_GHG <- gather(edgar_GHG,gas,value,CO2:SF6)
#edgar_GHG <- edgar_GHG %>% filter(value!=0)
#load('Data/edgar_data_gwp_ar6.RData')

# edgar6 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR_v6.0_emissions_GHG_1970_2018_IPCC contribution_2021.xlsx',
#                               sheet='EDGAR emissions',startRow=5)
# edgar6 <- gather(edgar6,year,value,Y_1970:Y_2018)
# 
# edgar6$year <- gsub("Y_","",edgar6$year)
# edgar6$value <- gsub("NULL",NA,edgar6$value)
# 
# edgar6 <- edgar6 %>% 
#   mutate(year=as.numeric(year)) %>% 
#   mutate(value=as.numeric(value))

############# make sure all code names are consistent
# 
# library(openxlsx)
# wb <- openxlsx::createWorkbook(title = "edgar_v5_v6_sector_codes")
# addWorksheet(wb,"edgar_v5_codes")
# addWorksheet(wb,"edgar_v6_codes")
# addWorksheet(wb,"matched_codes")
# addWorksheet(wb,"new_codes_not_matched")
# addWorksheet(wb,"old_codes_not_matched")
# 
# new_codes <- edgar6 %>%
#   #filter(year<2019) %>% 
#   filter(year==2018) %>% 
#   group_by(IPCC_code_1996,IPCC_code_description,fossil_bio) %>%
#   summarise(value=sum(value,na.rm=TRUE)) %>%
#   arrange(IPCC_code_1996) %>%
#   distinct() %>% 
#   select(sector_code_v6=IPCC_code_1996,description_v6=IPCC_code_description,fossil_bio,value_v6=value)
# 
# old_codes <- edgar_GHG %>% 
#   #filter(year<2019) %>%
#   filter(year==2018) %>% 
#   group_by(sector_code,description) %>%
#   summarise(value=sum(value,na.rm=TRUE)) %>%
#   arrange(sector_code) %>%
#   distinct() %>% 
#   select(sector_code_v5=sector_code,description_v5=description,value_v5=value)
# 
# ###### convert edgar v5 codes to edgar v6 codes
# 
# old_codes <- old_codes %>%
#   mutate(fossil_bio=ifelse(grepl("x",sector_code_v5),"bio","fossil")) %>% 
#   mutate(fossil_bio=ifelse(grepl("Field burning",description_v5),"bio",fossil_bio)) %>% 
#   mutate(fossil_bio=ifelse(grepl("Savannah fires",description_v5),"bio",fossil_bio))
# 
# old_codes$sector_code_v6 <- gsub("x","",old_codes$sector_code_v5)
# 
# matched_codes <- full_join(old_codes,new_codes,by=c("sector_code_v6","fossil_bio")) 
# 
# writeData(wb, sheet = "edgar_v5_codes", old_codes, colNames = T)
# writeData(wb, sheet = "edgar_v6_codes", new_codes, colNames = T)
# 
# not_new <- anti_join(new_codes,old_codes,by=c("sector_code_v6","fossil_bio")) 
# not_old <- anti_join(old_codes,new_codes,by=c("sector_code_v6","fossil_bio")) 
# 
# writeData(wb, sheet = "matched_codes", matched_codes, colNames = T)
# writeData(wb, sheet = "new_codes_not_matched", not_new, colNames = T)
# writeData(wb, sheet = "old_codes_not_matched", not_old, colNames = T)
# 
# saveWorkbook(wb,"Data/EDGAR/edgar_v5_v6_sector_codes.xlsx",overwrite = T)


#############

# load('Data/edgar6_data_gwp_ar6.RData')
# 
# ### which sectors change the most from 2018 to 2019
# 
# data <- edgar_GHG_ar6 %>% 
#   filter(year %in% c(2018,2019)) %>% 
#   group_by(year,sector_code,fossil_bio,description) %>% 
#   summarise(GHG=sum(GHG,na.rm=TRUE))
# 
# data <- spread(data,year,GHG)
# 
# data <- data %>% 
#   mutate(percent_difference =`2019`/`2018`)
# 
# 
# write.xlsx(data,'uhoh.xlsx')



############# check raw unprocessed total CO2 from each dataset
library(openxlsx)
wb <- openxlsx::createWorkbook(title = "edgar_v5FT_v6_comparison")
addWorksheet(wb,"total_co2")
addWorksheet(wb,"country_co2_2018")
addWorksheet(wb,"China_co2_2018")
addWorksheet(wb,"sector_co2_2018")

edgar6 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR_v6.0_emissions_GHG_1970_2018_IPCC contribution_2021.xlsx',
                              sheet='EDGAR emissions',startRow=5)
blarg <- edgar6 %>% select(Substance,Country_code_A3,IPCC_code_1996,IPCC_code_description,fossil_bio,Y_2018)
edgar6 <- gather(edgar6,year,value,Y_1970:Y_2018)

edgar6$year <- gsub("Y_","",edgar6$year)
edgar6$value <- gsub("NULL",NA,edgar6$value)

edgar6 <- edgar6 %>%
  mutate(year=as.numeric(year)) %>%
  mutate(value=as.numeric(value))

edgar6 <- edgar6 %>% 
  mutate(value=ifelse(fossil_bio=="bio" & Substance=="CO2",NA,value))


##

edgar5FT <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part A- CO2 (by JRC).xlsx',
                               sheet='CO2',startRow=10)
edgar5FT <- gather(edgar5FT,year,value,'1970':'2019')
edgar5FT <- edgar5FT %>% 
  select(ISO,year,EDGAR_country=Country,IPCC_for_std_report_detailed,IPCC_source_detailed_desc,CO2=value) %>% 
  mutate(year=as.numeric(year))

##

edgar6_total <- edgar6 %>% 
  filter(Substance=="CO2") %>% 
  group_by(year) %>% 
  summarise(tot_co2_edgar6=sum(value,na.rm=TRUE))

edgar5FT_total <- edgar5FT %>% 
  group_by(year) %>% 
  summarise(tot_co2_edgar5FT=sum(CO2,na.rm=TRUE))

total_co2 <- left_join(edgar6_total,edgar5FT_total,by="year")


writeData(wb, sheet = "total_co2", total_co2, colNames = T)

##

edgar6_country_co2 <- edgar6 %>% 
  filter(Substance=="CO2") %>% 
  filter(year==2018) %>% 
  group_by(Country_code_A3) %>% 
  summarise(co2_edgar6_2018=sum(value,na.rm=TRUE))

edgar5FT_country_co2 <- edgar5FT %>% 
  filter(year==2018) %>% 
  group_by(ISO) %>% 
  summarise(co2_edgar5FT_2018=sum(CO2,na.rm=TRUE))

country_co2 <- left_join(edgar6_country_co2,edgar5FT_country_co2,by=c("Country_code_A3"="ISO"))
country_co2 <- country_co2 %>% 
  mutate(difference=co2_edgar6_2018-co2_edgar5FT_2018)

writeData(wb, sheet = "country_co2_2018", country_co2, colNames = T)

## china

edgar6_CHN_co2 <- edgar6 %>% 
  filter(Substance=="CO2") %>% 
  filter(year==2018) %>% 
  filter(Country_code_A3=="CHN") %>% 
  mutate(edgar6_co2 = value) %>% 
  select(-value)

edgar5FT_CHN_co2 <- edgar5FT %>% 
  filter(year==2018) %>% 
  filter(ISO=="CHN") %>% 
  select(IPCC_for_std_report_detailed,edgar5FT_co2=CO2) %>% 
  mutate(fossil_bio="fossil")

CHN_co2 <- full_join(edgar6_CHN_co2,edgar5FT_CHN_co2,by=c("IPCC_code_1996"="IPCC_for_std_report_detailed","fossil_bio"))

writeData(wb, sheet = "China_co2_2018", CHN_co2, colNames = T)

## sectors

edgar6_sector_co2 <- edgar6 %>% 
  filter(Substance=="CO2") %>% 
  filter(year==2018) %>% 
  select(edgar6_description=IPCC_code_description,everything()) %>% 
  group_by(IPCC_code_1996,fossil_bio,edgar6_description) %>% 
  summarise(edgar6_co2_2018 = sum(value,na.rm=TRUE))

edgar5FT_sector_co2 <- edgar5FT %>% 
  filter(year==2014) %>% 
  select(edgar5FT_description=IPCC_source_detailed_desc,everything()) %>% 
  group_by(IPCC_for_std_report_detailed,edgar5FT_description) %>% 
  summarise(edgar5FT_co2_2018 = sum(CO2,na.rm=TRUE)) %>% 
  mutate(fossil_bio="fossil")

sector_co2 <- full_join(edgar6_sector_co2,edgar5FT_sector_co2,by=c("IPCC_code_1996"="IPCC_for_std_report_detailed","fossil_bio"))
sector_co2 <- sector_co2 %>% 
  select(IPCC_code_1996,fossil_bio,edgar6_description,edgar5FT_description,edgar6_co2_2018,edgar5FT_co2_2018)

writeData(wb, sheet = "sector_co2_2018", sector_co2, colNames = T)




saveWorkbook(wb,"Data/EDGAR/edgar_v5FT_v6_comparison.xlsx",overwrite = T)
