
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar_data_all.RData')

#################### prep

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(alpha.3,WB.income),by=c("ISO"="alpha.3"))

edgar_GHG <- edgar_GHG %>% 
  mutate(WB.income=ifelse(ISO=="AIR","Intl. Aviation",WB.income)) %>% 
  mutate(WB.income=ifelse(ISO=="SEA","Intl. Shipping",WB.income)) %>% 
  mutate(WB.income=ifelse(is.na(WB.income),"Low income",WB.income))



edgar_GHG <- edgar_GHG %>% 
  select(-region_ar6_5_short,-region_ar6_22,-region_ar6_dev) %>% 
  select(ISO,country,region_ar6_5,region_ar6_10,WB.income,year,everything()) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

load('Data/ipcc_categories.RData')
load('Data/gwps.RData')

edgar_categories <- edgar_GHG %>% 
  select(sector_code,chapter,chapter_title,description) %>% 
  unique() %>% 
  arrange(chapter)

regions <- edgar_GHG %>% 
  select(ISO,country,region_ar6_5,region_ar6_10,region_income=WB.income) %>% 
  distinct() %>% 
  arrange(ISO)


#################### aggregate by region and sector

regions_ar6_5 <- edgar_GHG %>% 
  group_by(region_ar6_5,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T)

regions_ar6_10 <- edgar_GHG %>% 
  group_by(region_ar6_10,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T)

regions_income <- edgar_GHG %>% 
  group_by(WB.income,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T)

sectors <- edgar_GHG %>% 
  group_by(chapter_title,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T) %>% 
  ungroup()
  

#################### add land to sectors
load("Data/land.RData")

land <- land %>% 
  group_by(year) %>% 
  summarise(mean=sum(mean))

land[,gwps$gas]=NA

land <- land %>% 
  filter(year>1969) %>% 
  filter(year<2019) %>% 
  mutate(chapter_title="AFOLU") %>% 
  mutate(CO2=mean) %>% 
  select(chapter_title,year,-mean,CO2:SF6)

sectors <- rbind(sectors,land)

### merge LUC CO2 and AFOLU
sectors <- sectors %>%
  group_by(chapter_title,year) %>%
  summarise_at(vars(gwps$gas),sum,na.rm=T) %>%
  ungroup()

#################### add land to regions

load("Data/land.RData")

land <- left_join(land,regions %>% select(region_ar6_5,region_ar6_10) %>% distinct(),by = "region_ar6_10")
land[,gwps$gas]=NA

land <- land %>% 
  filter(year>1969) %>% 
  filter(year<2019) %>% 
  mutate(CO2=mean)

land_5 <- land %>%
  group_by(year,region_ar6_5) %>% 
  summarise_at(vars(CO2:SF6),sum) %>% 
  select(region_ar6_5,year,CO2:SF6)

land_10 <- land %>% 
  group_by(year,region_ar6_10) %>% 
  summarise_at(vars(CO2:SF6),sum) %>% 
  select(region_ar6_10,year,CO2:SF6)

regions_ar6_5 <- rbind(regions_ar6_5,land_5)
regions_ar6_10 <- rbind(regions_ar6_10,land_10)

regions_ar6_5 <- regions_ar6_5 %>% 
  group_by(region_ar6_5,year) %>% 
  summarise_at(vars(CO2:SF6),sum,na.rm=TRUE)

regions_ar6_10 <- regions_ar6_10 %>% 
  group_by(region_ar6_10,year) %>% 
  summarise_at(vars(CO2:SF6),sum,na.rm=TRUE)

#################### info

info = data.frame("x" = c("Units","Source"),
                  "y" = c("All units in tons of native units (no GWPs applied)",
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))


#################### full file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_warming")
addWorksheet(wb,"info")
addWorksheet(wb,"regions_ar6_5")
addWorksheet(wb,"regions_ar6_10")
addWorksheet(wb,"regions_income")
addWorksheet(wb,"sectors")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "regions_ar6_5", regions_ar6_5, colNames = T)
writeData(wb, sheet = "regions_ar6_10", regions_ar6_10, colNames = T)
writeData(wb, sheet = "regions_income", regions_income, colNames = T)
writeData(wb, sheet = "sectors", sectors, colNames = T)
writeData(wb, sheet = "sector_classification",edgar_categories,colNames=T)
writeData(wb, sheet = "region_classification",regions,colNames=T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_warming_21_09.xlsx",overwrite = T)


  
  
  
  