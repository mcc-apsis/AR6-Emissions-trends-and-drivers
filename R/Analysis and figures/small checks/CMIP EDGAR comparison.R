

rm(list = ls())
library(tidyverse)
#load('Data/edgar6_v5_data_raw.RData')
#load('Data/edgar6_v5_data_ghg_gwp_ar6.RData')
load('Data/ipcc_data.RData')

load('Data/gwps.RData')
load("Data/land.RData")


cmip <- read.csv('Data/Not public/supplemetary data/history_ar6_harmonization.csv')

cmip <- cmip %>% 
  mutate(gas=ifelse(Variable=="AR6 climate diagnostics|Emissions|CO2|AFOLU|Unharmonized","CO2 LULUCF",NA)) %>% 
  mutate(gas=ifelse(Variable=="AR6 climate diagnostics|Emissions|CO2|Energy and Industrial Processes|Unharmonized","CO2",gas)) %>% 
  mutate(gas=ifelse(Variable=="AR6 climate diagnostics|Emissions|CH4|Unharmonized","CH4",gas)) %>% 
  mutate(gas=ifelse(Variable=="AR6 climate diagnostics|Emissions|N2O|Unharmonized","N2O",gas)) %>% 
  mutate(gas=ifelse(Variable=="AR6 climate diagnostics|Emissions|F-Gases|Unharmonized","Fgas",gas))

cmip <- cmip %>% 
  filter(!is.na(gas))


cmip <- gather(cmip,year,value,X1750:X2015)
cmip$year <- gsub("X","",cmip$year)
cmip <- cmip %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year>1989)

cmip <- cmip %>% 
  mutate(value = ifelse(grepl("Mt",Unit),value*1e6,value)) %>% 
  mutate(value = ifelse(grepl("kt",Unit),value*1000,value))

# cmip$Unit <- gsub("Mt ","",cmip$Unit)
# cmip$Unit <- gsub("kt ","",cmip$Unit)
# cmip$Unit <- gsub("/yr","",cmip$Unit)

cmip <- cmip %>% 
  select(gas,year,CMIP=value)
  filter(year>1989)


edgar_raw <- edgar_raw %>%
  group_by(gas,year) %>% 
  summarise(edgar_v6=sum(value,na.rm=TRUE)) %>% 
  filter(year>1989) %>% 
  filter(year<2016)

data <- left_join(cmip,edgar_raw)

edgar_fgas <- edgar_ghg %>% 
  group_by(year) %>% 
  summarise(edgar_v6_fgas = sum(Fgas,na.rm=TRUE)) %>% 
  mutate(gas="Fgas")

data <- left_join(data,edgar_fgas)
data <- data %>% 
  mutate(edgar_v6=ifelse(gas=="Fgas",edgar_v6_fgas,edgar_v6)) %>% 
  select(-edgar_v6_fgas)

land <- land %>% 
  group_by(year) %>% 
  summarise(GCB_2020=sum(mean,na.rm=TRUE)) %>% 
  mutate(gas="CO2 LULUCF")

data <- left_join(data,land)



data <- data %>% 
  mutate(ch2=edgar_v6) %>% 
  mutate(ch2=ifelse(gas=="CO2 LULUCF",GCB_2020,ch2)) %>% 
  select(-edgar_v6,-GCB_2020)

diff <- data

wb <- openxlsx::createWorkbook(title = "blarg")

openxlsx::addWorksheet(wb,"data")
openxlsx::writeData(wb,"data",data, colNames = T, rowNames = F)



data <- gather(data,dataset,value,CMIP:ch2)

data %>% 
  ggplot(.,aes(x=year,y=value,colour=dataset)) +
  geom_line() +
  facet_wrap(.~gas,scales="free")

diff <- diff %>% 
  filter(year==2015)

diff <- left_join(diff,gwps %>% select(gas,gwp_ar6))
diff <- diff %>% 
  mutate(gwp_ar6=ifelse(gas=="CH4",32,gwp_ar6))


openxlsx::addWorksheet(wb,"differences")
openxlsx::writeData(wb,"differences",diff, colNames = T, rowNames = F)

openxlsx::saveWorkbook(wb,"cmip_edgar_comparison.xlsx",overwrite=T)
