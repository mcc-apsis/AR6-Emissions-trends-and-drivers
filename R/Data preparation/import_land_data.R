rm(list = ls())
library(tidyverse)
library(openxlsx)

#blue <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "BLUE_GCB2019_IPCC_regions",startRow=1,colNames=TRUE)
#houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "H&N_2017_IPCC_regions",startRow=1,colNames=TRUE)
#houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020_HNExtrapolated.xlsx",sheet = "data",startRow=1,colNames=TRUE)

blue <- read.xlsx("Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="BLUE_GCB2020_IPCC_regions")
houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="H&N_2017_IPCC_regions")
oscar <- read.xlsx("Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="OSCAR_IPCC_regions")

blue <- gather(blue,region_ar6_10,blue,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,blue)
houghton <- gather(houghton,region_ar6_10,houghton,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,houghton)
oscar <- gather(oscar,region_ar6_10,oscar,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,oscar)

land <- left_join(blue,houghton,by = c("region_ar6_10", "year"))
land <- left_join(land,oscar,by = c("region_ar6_10", "year"))

# units are TgC. Convert to t CO2.
land <- land %>% 
  mutate(blue=blue*1e6) %>% 
  mutate(blue=blue*3.664) %>% 
  mutate(houghton=houghton*1e6) %>% 
  mutate(houghton=houghton*3.664) %>% 
  mutate(oscar=oscar*1e6) %>% 
  mutate(oscar=oscar*3.664)
land <- land %>% 
  mutate(mean=(blue+houghton+oscar)/3)

# check country names and regions
land$region_ar6_10 <- gsub("[.]"," ",land$region_ar6_10)

load('Data/ipcc_regions.RData')

regions <- ipcc_regions %>% select(region_ar6_10) %>% distinct()

uhoh <- anti_join(land,regions,by="region_ar6_10")

wb <- createWorkbook()
info = data.frame(x=c("Author","Units","Date","Contact"),y=c("Julia Pongratz","tCO2",as.character(Sys.Date()),"Lamb@mcc-berlin.net"))
addWorksheet(wb,"info")
openxlsx::writeData(wb, sheet = "info", info, colNames = F, rowNames = F)
addWorksheet(wb,"data")
openxlsx::writeData(wb, sheet = "data", land, colNames = T, rowNames = F)

openxlsx::saveWorkbook(wb,"Results/Data/ipcc_ar6_land_data.xlsx",overwrite=T)

save(land,file='Data/land.RData')
