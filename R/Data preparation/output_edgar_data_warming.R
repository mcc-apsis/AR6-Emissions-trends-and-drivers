
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar6_v5_data_raw.RData')

#################### prep

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_raw <- left_join(edgar_raw,codes %>% select(alpha.3,WB.income),by=c("ISO"="alpha.3"))

edgar_raw <- edgar_raw %>% 
  mutate(WB.income=ifelse(ISO=="AIR","Intl. Aviation",WB.income)) %>% 
  mutate(WB.income=ifelse(ISO=="SEA","Intl. Shipping",WB.income)) %>% 
  mutate(WB.income=ifelse(is.na(WB.income),"Low income",WB.income))

edgar_raw <- edgar_raw %>% 
  select(-region_ar6_22,-region_ar6_dev) %>% 
  select(ISO,country,region_ar6_6,region_ar6_10,WB.income,year,everything()) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

load('Data/ipcc_sectors.RData')
load('Data/gwps.RData')

edgar_categories <- edgar_raw %>% 
  select(sector_code,chapter,chapter_title,description) %>% 
  unique() %>% 
  arrange(chapter)

regions <- edgar_raw %>% 
  select(ISO,country,region_ar6_6,region_ar6_10,region_income=WB.income) %>% 
  distinct() %>% 
  arrange(ISO)


#################### aggregate by region and sector

regions_ar6_6 <- edgar_raw %>% 
  group_by(region_ar6_6,year,gas) %>% 
  summarise(value=sum(value,na.rm = T))


regions_ar6_10 <- edgar_raw %>% 
  group_by(region_ar6_10,year,gas) %>% 
  summarise(value=sum(value,na.rm = T))


regions_income <- edgar_raw %>% 
  group_by(WB.income,year,gas) %>% 
  summarise(value=sum(value,na.rm = T))


sectors <- edgar_raw %>% 
  group_by(chapter_title,year,gas) %>% 
  summarise(value=sum(value,na.rm = T))

#################### add land to sectors
load("Data/land.RData")

land <- land %>% 
  group_by(year) %>%
  summarise(value=sum(mean))


land <- land %>% 
  filter(year>1969) %>% 
  filter(year<2020) %>% 
  mutate(chapter_title="AFOLU") %>% 
  mutate(gas="CO2 LULUCF")

sectors <- rbind(sectors,land)

#################### add land to regions

load("Data/land.RData")

land <- land %>% 
  filter(year>1969) %>% 
  filter(year<2020) %>% 
  mutate(gas="CO2 LULUCF") %>% 
  mutate(value=mean)

land_5 <- land %>%
  group_by(year,region_ar6_6,gas) %>% 
  summarise(value=sum(value))


land_10 <- land %>% 
  group_by(year,region_ar6_10,gas) %>% 
  summarise(value=sum(value))


regions_ar6_6 <- rbind(regions_ar6_6,land_5)
regions_ar6_10 <- rbind(regions_ar6_10,land_10)

#################### spread


regions_ar6_6 <- spread(regions_ar6_6,gas,value)
regions_ar6_10 <- spread(regions_ar6_10,gas,value)
regions_income <- spread(regions_income,gas,value)
sectors <- spread(sectors,gas,value)



#################### info

info = data.frame("x" = c("Units","Source"),
                  "y" = c("All units in tons of native units (no GWPs applied)",
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))


#################### full file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_warming")
addWorksheet(wb,"info")
addWorksheet(wb,"regions_ar6_6")
addWorksheet(wb,"regions_ar6_10")
addWorksheet(wb,"regions_income")
addWorksheet(wb,"sectors")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "regions_ar6_6", regions_ar6_6, colNames = T)
writeData(wb, sheet = "regions_ar6_10", regions_ar6_10, colNames = T)
writeData(wb, sheet = "regions_income", regions_income, colNames = T)
writeData(wb, sheet = "sectors", sectors, colNames = T)
writeData(wb, sheet = "sector_classification",edgar_categories,colNames=T)
writeData(wb, sheet = "region_classification",regions,colNames=T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_warming_13_10_21.xlsx",overwrite = T)


  
  
  
  