rm(list = ls())
library(zoo)
library(openxlsx)
library(tidyverse)


#ipcc_sectors <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDITABLE.xlsx',
#                                'ar6_sector_classification')
# 
# ipcc_sectors <- ipcc_sectors %>% 
#   select(code:description,subsector=subsector_suggestion_Lamb,subsector_title=subsector_title_suggestion_Lamb)

ipcc_sectors <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDGARv6_FGD.xlsx',
                               'ar6_sector_classification')

save(ipcc_sectors,file='Data/ipcc_sectors.RData')



###



ipcc_regions <- read.xlsx('Data/Codes and classifications/Country categories plus alpha codes 27-04-2021.xlsx','Breakdown_list_dev_level')

ipcc_regions <- ipcc_regions %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Developed Countries","DEV",NA)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Latin America and Caribbean","LAM",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Africa","AFR",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Middle East","MEA",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Eastern Europe and West-Central Asia","EEA",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Asia and Developing Pacific","APC",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Intl. Shipping","SEA",region_ar6_6_short)) %>% 
  mutate(region_ar6_6_short=ifelse(region_ar6_6=="Intl. Aviation","AIR",region_ar6_6_short))



regions_10 <- ipcc_regions %>% 
  select(region_ar6_10) %>% 
  distinct() %>% 
  arrange(region_ar6_10)

regions_10 <- cbind(regions_10,
                    data.frame(region_ar6_10_short=c(
                      "Africa",
                      "A. Pacific",
                      "E. Asia",
                      "Eurasia",
                      "Europe",
                      "Latin Am.",
                      "Middle E.",
                      "N. America",
                      "S.E. Asia",
                      "S. Asia")))

ipcc_regions <- left_join(ipcc_regions,regions_10,by = "region_ar6_10")

save(ipcc_regions,file='Data/ipcc_regions.RData')


###


gwps <- openxlsx::read.xlsx('Data/Codes and classifications/gwps.xlsx','gwps')

gwps_ch4 <- openxlsx::read.xlsx('Data/Codes and classifications/gwps.xlsx','ch4 EDGARv6')

save(gwps,gwps_ch4,file='Data/gwps.RData')



# what is biogenic?

# load('Data/edgar_data_gwp_ar6.RData')
# blarg <- edgar_GHG_ar6 %>% 
#   group_by(chapter,chapter_title,subsector,sector_code,description) %>% 
#   summarise(CH4=sum(CH4,na.rm=TRUE)) #%>% 
# 
# blarg <- blarg %>% 
#   filter(CH4!=0)
# 
# openxlsx::write.xlsx(x=blarg,file="CH4_sources.xlsx",sheetName = "data",row.names=FALSE)
