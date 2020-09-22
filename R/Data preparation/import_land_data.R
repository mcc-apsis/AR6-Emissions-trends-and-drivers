rm(list = ls())
library(tidyverse)
library(openxlsx)

blue <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "BLUE_GCB2019_IPCC_regions",startRow=1,colNames=TRUE)
#houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "H&N_2017_IPCC_regions",startRow=1,colNames=TRUE)
houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020_HNExtrapolated.xlsx",sheet = "data",startRow=1,colNames=TRUE)



blue <- gather(blue,region_ar6_10,blue,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,blue)
houghton <- gather(houghton,region_ar6_10,houghton,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,houghton)

land <- left_join(blue,houghton,by = c("region_ar6_10", "year"))

# bring forward Houghton from 2015 (WAITING ON EXTRAPOLATION FROM JULIA)
# land <- land %>% 
#   fill(houghton)


# units are TgC. Convert to t CO2.
land <- land %>% 
  mutate(blue=blue*1e6) %>% 
  mutate(blue=blue*3.664) %>% 
  mutate(houghton=houghton*1e6) %>% 
  mutate(houghton=houghton*3.664)
land <- land %>% 
  mutate(mean=(blue+houghton)/2)

# check country names and regions
land$region_ar6_10 <- gsub("[.]"," ",land$region_ar6_10)

load('Data/tsu_codes.RData')

regions <- tsu_codes %>% select(region_ar6_10) %>% distinct()

uhoh <- anti_join(land,regions,by="region_ar6_10")

save(land,file='Data/land.RData')
