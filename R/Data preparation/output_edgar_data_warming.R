
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
  select(-category_1,-category_2,-category_3,-region_ar6_5_short,-region_ar6_10,-region_ar6_22,-region_ar6_dev) %>% 
  select(ISO,country,region_ar6_5,WB.income,year,everything()) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

load('Data/ipcc_categories.RData')
load('Data/gwps.RData')

edgar_categories <- edgar_GHG %>% 
  select(sector_code,chapter,chapter_title,description) %>% 
  unique() %>% 
  arrange(chapter)

regions <- edgar_GHG %>% 
  select(ISO,country,region_ar6_5,region_income=WB.income) %>% 
  distinct() %>% 
  arrange(ISO)


#################### aggregate by region and sector

regions_ar6 <- edgar_GHG %>% 
  group_by(region_ar6_5,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T)

regions_income <- edgar_GHG %>% 
  group_by(WB.income,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T)

sectors <- edgar_GHG %>% 
  group_by(chapter_title,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm = T) %>% 
  ungroup()
  

#################### 

land_data <- openxlsx::read.xlsx('Data/Global_Carbon_Budget_2019v1.0.xlsx',sheet="Global Carbon Budget",rows=19:79,cols=2:4) %>% 
  select(year=Year,value="land-use.change.emissions")

#### convert from C to CO2

land_data <- land_data %>% 
  mutate(value=value*(44/12)) %>%
  mutate(value=value*1e9)

land_data[,gwps$gas]=NA

land_data <- land_data %>% 
  filter(year>1969) %>% 
  filter(year<2019) %>% 
  mutate(chapter_title="AFOLU") %>% 
  mutate(CO2=value) %>% 
  select(chapter_title,year,everything(),-value)

sectors <- rbind(sectors,land_data)

### merge LUC CO2 and AFOLU
sectors <- sectors %>% 
  group_by(chapter_title,year) %>% 
  summarise_at(vars(gwps$gas),sum,na.rm=T) %>% 
  ungroup()


#################### info

info = data.frame("x" = c("Units","Source"),
                  "y" = c("All units in tons of native units (no GWPs applied)",
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))


#################### full file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_warming")
addWorksheet(wb,"info")
addWorksheet(wb,"regions_ar6")
addWorksheet(wb,"regions_income")
addWorksheet(wb,"sectors")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "regions_ar6", regions_ar6, colNames = T)
writeData(wb, sheet = "regions_income", regions_income, colNames = T)
writeData(wb, sheet = "sectors", sectors, colNames = T)
writeData(wb, sheet = "sector_classification",edgar_categories,colNames=T)
writeData(wb, sheet = "region_classification",regions,colNames=T)



saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_warming.xlsx",overwrite = T)



  
  
  
  