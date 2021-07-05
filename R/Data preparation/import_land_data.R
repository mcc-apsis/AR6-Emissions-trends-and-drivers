rm(list = ls())
library(tidyverse)
library(openxlsx)

#blue <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "BLUE_GCB2019_IPCC_regions",startRow=1,colNames=TRUE)
#houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020.xlsx",sheet = "H&N_2017_IPCC_regions",startRow=1,colNames=TRUE)
#houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_26082020_HNExtrapolated.xlsx",sheet = "data",startRow=1,colNames=TRUE)

#blue_old <- read.xlsx("../../Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="BLUE_GCB2020_IPCC_regions")
#houghton <- read.xlsx("../../Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="H&N_2017_IPCC_regions")
#oscar <- read.xlsx("../../Data/Land and GCB/Country_ELUC_25112020_forWill.xlsx",sheet="OSCAR_IPCC_regions")

blue <- read.xlsx("Data/Land and GCB/Country_ELUC_23032021.xlsx",sheet="BLUE_GCB2020_IPCC_regions")
houghton <- read.xlsx("Data/Land and GCB/Country_ELUC_23032021.xlsx",sheet="H&N_2017_IPCC_regions")
oscar <- read.xlsx("Data/Land and GCB/Country_ELUC_23032021.xlsx",sheet="oscar_10ipccregions")

blue <- gather(blue,region_ar6_10,blue,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,blue) %>%
  filter(!is.na(year))
houghton <- gather(houghton,region_ar6_10,houghton,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,houghton) %>%
  filter(!is.na(year))
oscar <- gather(oscar,region_ar6_10,oscar,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,oscar) %>%
  filter(!is.na(year))

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

regions <- ipcc_regions %>% select(region_ar6_10,region_ar6_6,region_ar6_10_short,region_ar6_6_short) %>% distinct()

uhoh <- anti_join(land,regions,by="region_ar6_10")

land <- left_join(land,regions,by="region_ar6_10")

land <- land %>% 
  select(year,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,blue,houghton,oscar,mean) %>% 
  arrange(region_ar6_6,region_ar6_10,year)


#check updated data vs. previous data
# colnames(land_old)[3:6]<-paste(colnames(land_old)[3:6],"_old",sep = "")
# comp<-left_join(land,land_old,by=c("year","region_ar6_10"))
# p1<-comp %>% 
#   filter(region_ar6_10==regions[2,1])%>%
#   ggplot() + 
#   geom_line(mapping=aes(x=year,y=blue))+
#   geom_line(mapping=aes(x=year,y=blue_old,color="red"))
# p1
# 
# check <-comp %>% 
#   filter(region_ar6_10=="South-East Asia and Developing Pacific")%>%
#   filter(year>1975) %>%
#   mutate(blue_diff=blue-blue_old) %>%
#   mutate(blue_diff_rate=(blue-blue_old)/blue_old) %>%
#   select(region_ar6_10,year,blue,blue_old,blue_diff,blue_diff_rate)

wb <- createWorkbook()
info = data.frame(x=c("Author","Units","Last update","Contact","Code"),y=c("Julia Pongratz",
    "tCO2",
    as.character(Sys.Date()),
    "Lamb@mcc-berlin.net",
    "https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers/blob/master/R/Data%20preparation/import_land_data.R"))
addWorksheet(wb,"info")
openxlsx::writeData(wb, sheet = "info", info, colNames = F, rowNames = F)
addWorksheet(wb,"data")
openxlsx::writeData(wb, sheet = "data", land, colNames = T, rowNames = F)

openxlsx::saveWorkbook(wb,"Results/Data/ipcc_ar6_data_land_co2.xlsx",overwrite=T)

save(land,file='Data/land.RData')
