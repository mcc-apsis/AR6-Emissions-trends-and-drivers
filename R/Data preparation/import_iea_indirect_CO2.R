rm(list = ls())
library(tidyverse)
library(openxlsx)

######### read IEA data and clean empty country rows

iea <- read.csv(file="Data/IEA/2020 update/iea_scope2.csv",sep = ";")
names <- c("country","flow","var",1990:2018)
names(iea) <- names
iea <- iea %>% 
  filter(country!="COUNTRY") %>% 
  filter(row_number()!=19553:19656)

######### tidy up variables, convert to long format, change units to Gt CO2

iea <- gather(iea,year,value,`1990`:`2018`)
iea <- iea %>% 
  mutate(value=ifelse(grepl("x",value),NA,value)) %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(var=as.character(var)) %>% 
  mutate(year=as.numeric(year))

iea <- iea %>% 
  mutate(var=ifelse(grepl("Emissions by sector",var),"CO2",var)) %>% 
  mutate(var=ifelse(grepl("Emissions with electricity and heat",var),"CO2_plus_elec_heat",var)) %>% 
  filter(var=="CO2" | var=="CO2_plus_elec_heat") %>% 
  mutate(value=value*1000) %>%
  mutate(value=value/1e9)

######### match IEA sectors to (EDGAR) IPCC 2006 codes

matching <- openxlsx::read.xlsx("Data/Codes and classifications/iea_ipcc_sector_classification.xlsx",sheet = "sectors")
matching <- matching %>% 
  select(CODE,LEVEL,ipcc.code,sector_chapter,chapter_title,description)
iea <- left_join(iea,matching %>% select(CODE,LEVEL,ipcc.code),by=c("flow"="CODE"))

######### save world totals for ELECHEAT sector

world_totals <- iea %>% 
  filter(country=="World") %>% 
  filter(flow=="ELECHEAT") %>% 
  filter(var=="CO2") %>% 
  select(year,world_elec_heat=value)

######### some checks and coordination with IEA authors

# blarg <- spread(iea,var,value) %>% 
#   filter(year==2018) %>% 
#   filter(country=="World")
# blarg <- blarg %>% 
#   mutate(indirect=CO2_plus_elec_heat-CO2) %>% 
#   mutate(indirect=ifelse(grepl("TOTIND",flow),NA,indirect)) %>% 
#   mutate(indirect=ifelse(grepl("TOTTRANS",flow),NA,indirect)) %>% 
#   mutate(indirect=ifelse(grepl("CO2FCOMB",flow),NA,indirect))

# avi <- spread(iea,var,value) %>% 
#   filter(year==2018) %>% 
#   filter(country=="Shipping and Aviation")
# avi <- avi %>% 
#   mutate(indirect=CO2_plus_elec_heat-CO2) %>% 
#   mutate(indirect=ifelse(grepl("TOTIND",flow),NA,indirect)) %>% 
#   mutate(indirect=ifelse(grepl("TOTTRANS",flow),NA,indirect)) %>% 
#   mutate(indirect=ifelse(grepl("CO2FCOMB",flow),NA,indirect))

#xlsx::write.xlsx(blarg,file="Data/iea_indirect_world.xlsx",col.names = TRUE,row.names = FALSE)


######### use IPCC 2006 codes to merge IEA with AR6 sectors and subsectors

load("Data/ipcc_sectors.RData")
iea_trimmed <- left_join(iea,ipcc_sectors %>% select(-IPCC.2006),by=c("ipcc.code"="code"))

########### !!!!! here is where things go wrong - OTHEN and ONONSPEC are removed
iea_trimmed <- iea_trimmed %>% 
  filter(!is.na(ipcc.code))

######### identify IPCC regions in the IEA data and remove rest

load("Data/tsu_codes.RData")
regions_10 <- tsu_codes %>% 
  select(region_ar6_10,region_ar6_5) %>% 
  distinct() %>%
  mutate(include=1)

iea_trimmed <- left_join(iea_trimmed,regions_10,by=c("country"="region_ar6_10"))
iea_trimmed <- iea_trimmed %>% 
  filter(include==1)

######### calculate % of world ELECHEAT indirect CO2 for each region/subsector

iea_trimmed <- spread(iea_trimmed,var,value)
iea_trimmed <- iea_trimmed %>% 
  mutate(indirect_CO2 = CO2_plus_elec_heat-CO2)

iea_trimmed <- left_join(iea_trimmed,world_totals,by="year")
iea_trimmed <- iea_trimmed %>% 
  mutate(indirect_fraction = indirect_CO2/world_elec_heat) %>% 
  mutate(indirect_fraction = ifelse(indirect_CO2==0,NA,indirect_fraction))

######### join EDGAR world ELECHEAT and propagate adjusted indirect CO2

load("Data/edgar_data_gwp_ar6.RData")
edgar_world_elec_heat <- edgar_GHG_ar6 %>% 
  group_by(year,chapter,subsector,subsector_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE) %>% 
  ungroup()

edgar_world_elec_heat <- edgar_world_elec_heat %>% 
  filter(year>1989) %>% 
  filter(subsector_title=="Electricity & heat") %>% 
  mutate_at(vars(CO2:GHG),list(~./1e9))

iea_trimmed <- left_join(iea_trimmed,edgar_world_elec_heat %>% select(year,edgar_world_elec_heat=CO2),
                         by="year")

iea_trimmed <- iea_trimmed %>% 
  mutate(CO2_indirect=edgar_world_elec_heat*indirect_fraction) %>% 
  filter(!is.na(CO2_indirect))

######### tidy up and save

indirect_CO2 <- iea_trimmed %>% 
  select(region_ar6_10=country,region_ar6_5,year,chapter=IPCC_AR6_chapter,
         chapter_title=IPCC_AR6_chapter_title,sector_code=ipcc.code,description,
         subsector,subsector_title,CO2_indirect)

save(indirect_CO2,file="Data/indirect_CO2.RData")
