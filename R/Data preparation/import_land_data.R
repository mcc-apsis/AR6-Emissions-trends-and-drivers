rm(list = ls())
library(tidyverse)
library(openxlsx)

# FGD version

blue_fgd <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_23032021.xlsx",sheet="BLUE_GCB2020_IPCC_regions")
houghton_fgd <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_23032021.xlsx",sheet="H&N_2017_IPCC_regions")
oscar_fgd <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_23032021.xlsx",sheet="oscar_10ipccregions")


blue_fgd <- gather(blue_fgd,region_ar6_10,blue,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,blue) %>%
  filter(!is.na(year))
houghton_fgd <- gather(houghton_fgd,region_ar6_10,houghton,`Africa`:`Southern.Asia`) %>% 
  select(region_ar6_10,year=X1,houghton) %>%
  filter(!is.na(year))
oscar_fgd <- gather(oscar_fgd,region_ar6_10,oscar,`Africa`:`Southern.Asia`) %>%
  select(region_ar6_10,year=X1,oscar) %>%
  filter(!is.na(year))

land_fgd <- left_join(blue_fgd,houghton_fgd,by = c("region_ar6_10", "year"))
land_fgd <- left_join(land_fgd,oscar_fgd,by = c("region_ar6_10", "year"))

land_fgd <- land_fgd %>% 
  mutate(blue=blue*1e6) %>% 
  mutate(blue=blue*3.664) %>% 
  mutate(houghton=houghton*1e6) %>% 
  mutate(houghton=houghton*3.664) %>% 
  mutate(oscar=oscar*1e6) %>% 
  mutate(oscar=oscar*3.664)
land_fgd <- land_fgd %>% 
  mutate(mean=(blue+houghton+oscar)/3)

totals_fgd <- land_fgd %>% 
  group_by(year) %>% 
  summarise_at(vars(blue,houghton,oscar,mean),sum,na.rm=TRUE)

totals_fgd <- gather(totals_fgd,var,value_old,-year)

# SPM plenary version


blue <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_GCB2020_24022022.xlsx",sheet="BLUE_GCB2020_IPCC_regions")
houghton <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_GCB2020_24022022.xlsx",sheet="H&N_2017_IPCC_regions")
oscar <- read.xlsx("Data/Not public/Land and GCB/Country_ELUC_GCB2020_24022022.xlsx",sheet="oscar_10ipccregions")

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

##

totals <- land %>% 
  group_by(year) %>% 
  summarise_at(vars(blue,houghton,oscar,mean),sum,na.rm=TRUE)

totals <- gather(totals,var,value_new,-year)

totals_compare <- left_join(totals,totals_fgd,by=c("year","var"))
totals_compare <- totals_compare %>% 
  mutate(difference=value_new-value_old)

write.xlsx(totals_compare,"land_co2_comparison.xlsx")



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

save(land,file='Data/data_land_co2.RData')
