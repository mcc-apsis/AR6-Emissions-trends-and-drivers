rm(list = ls())
library(tidyverse)
library(openxlsx)

load('Data/edgar6_data_raw.RData')
edgar_raw_6 <- edgar_raw
load('Data/edgar5_data_raw.RData')
edgar_raw_5 <- edgar_raw

load('Data/edgar6_data_ghg_gwp_ar6.RData')
edgar_ghg_6 <- edgar_ghg
load('Data/edgar5_data_ghg_gwp_ar6.RData')
edgar_ghg_5 <- edgar_ghg

load('Data/land.RData')

rm(edgar_raw,edgar_ghg)


## identify Fgas sectors

fgas_sectors_ar5 <- edgar_ghg_5 %>% 
  filter(!is.na(Fgas)) %>% 
  group_by(sector_code,fossil_bio,description) %>% 
  #summarise_at(vars(Fgas,CO2,CH4,N2O),sum,na.rm=TRUE) %>% 
  summarise(fgas_5=sum(Fgas,na.rm=TRUE)) %>% 
  distinct()

fgas_sectors_ar6 <- edgar_ghg_6 %>% 
  filter(!is.na(Fgas)) %>% 
  group_by(sector_code,fossil_bio,description) %>% 
  #summarise_at(vars(Fgas,CO2,CH4,N2O),sum,na.rm=TRUE) %>% 
  summarise(fgas_6=sum(Fgas,na.rm=TRUE)) %>% 
  distinct()

fgas_sectors <- full_join(fgas_sectors_ar6,fgas_sectors_ar5)

## 2C3a has CO2 emissions in it. Can I just delete that too and put in EDGAR v5? (This would be MUCH EASIER)

alu_5 <- edgar_ghg_5 %>% filter(sector_code=="2C3a") %>% group_by(year) %>% summarise(CO2_5=sum(CO2,na.rm=TRUE)) 
alu_6 <- edgar_ghg_6 %>% filter(sector_code=="2C3a") %>% group_by(year) %>% summarise(CO2_6=sum(CO2,na.rm=TRUE)) 
alu <- left_join(alu_5,alu_6,by="year")
alu <- gather(alu,version,value,-year)
alu %>% ggplot(.,aes(x=year,y=value/1e9,color=version)) + geom_path()

###################### PRETTY MINOR DIFFERENCES, GOING TO SWAP ALU CO2 AS WELL ###################### 

## remove all fgas sectors in AR6 (including 2C3a CO2!)

edgar_ghg_essd <- left_join(edgar_ghg_6,fgas_sectors_ar6 %>% select(-fgas_6) %>% mutate(remove=1),
                            by = c("sector_code", "fossil_bio", "description"))

edgar_ghg_essd <- edgar_ghg_essd %>% 
  filter(is.na(remove)) %>% 
  select(-remove)

edgar_raw_essd <- left_join(edgar_raw_6,fgas_sectors_ar6 %>% select(-fgas_6) %>% mutate(remove=1),
                            by = c("sector_code", "fossil_bio", "description"))

edgar_raw_essd <- edgar_raw_essd %>% 
  filter(is.na(remove)) %>% 
  select(-remove)


## identify fgas sectors in AR5

edgar_ghg_5_fgas <- left_join(edgar_ghg_5,fgas_sectors_ar5 %>% select(-fgas_5) %>% mutate(keep=1),
                              by = c("sector_code", "fossil_bio", "description"))

edgar_ghg_5_fgas <- edgar_ghg_5_fgas %>% 
  filter(keep==1) %>% 
  select(-keep,-sector_code_v5,-description_v5)

edgar_raw_5_fgas <- left_join(edgar_raw_5,fgas_sectors_ar5 %>% select(-fgas_5) %>% mutate(keep=1),
                              by = c("sector_code", "fossil_bio", "description"))

edgar_raw_5_fgas <- edgar_raw_5_fgas %>% 
  filter(keep==1) %>% 
  select(-keep,-sector_code_v5,-description_v5)


## bind new data

edgar_ghg_essd <- rbind(edgar_ghg_essd,edgar_ghg_5_fgas)
edgar_raw_essd <- rbind(edgar_raw_essd,edgar_raw_5_fgas)

edgar_ghg_essd <- edgar_ghg_essd %>% 
  arrange(ISO,year,sector_code)

edgar_raw_essd <- edgar_raw_essd %>% 
  arrange(ISO,year,sector_code)

## save

edgar_raw <- edgar_raw_essd
save(edgar_raw,file='Data/edgar_essd_data_raw.RData')

edgar_ghg <- edgar_ghg_essd
save(edgar_ghg,file='Data/edgar_essd_data_ghg_gwp_ar6.RData')
