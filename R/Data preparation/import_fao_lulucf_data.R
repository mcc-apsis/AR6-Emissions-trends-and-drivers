
rm(list = ls())
library(tidyverse)
library(openxlsx)


country_codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
load('Data/ipcc_regions.RData')



fao_land <- read.xlsx('Data/Land and GCB/FAOSTAT_land_use.xlsx')



fao_land <- fao_land %>% 
  select(country=Area,year=Year,var=Item,value=Value) %>% 
  mutate(country=tolower(country))

fao_land <- left_join(fao_land,country_codes,by=c("country"="alternative.name"))
not_joined <- anti_join(fao_land,country_codes,by=c("country"="alternative.name"))

fao_land <- left_join(fao_land,ipcc_regions,by=c("alpha.3"="ISO"))

## note there are a few historical countries in there not mapped

# change gigagrams to tons (1000 to 1)

fao_land <- fao_land %>% 
  mutate(value=value*1000)

# remove the second china

fao_land <- fao_land %>% 
  filter(country!="china, mainland") %>% 
  filter(country!="world") %>% 
  filter(var!="Land Use total") %>% 
  
  save(fao_land,file="Data/fao_land.RData")




blarg <- fao_land %>% 
  filter(country!="world") %>% 
  filter(var!="Land Use total") %>% 
  group_by(year) %>% 
  summarise(value_bottomup=sum(value,na.rm=TRUE)/1e9)

blarg2 <- fao_land %>% 
  filter(country=="world") %>% 
  filter(var=="Land Use total") %>% 
  mutate(value=value/1e9) %>% 
  select(year,world_total_value=value)

blarg2 %>% ggplot(.,aes(x=year,y=value/1e9)) +
  geom_path() +
  ylim(-1,9) +
  xlim(1960,2020)

blarg <- left_join(blarg,blarg2)

mean <- blarg %>% 
  filter(year>=2009) %>% 
  filter(year<=2018)
