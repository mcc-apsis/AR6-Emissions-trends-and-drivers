
rm(list = ls())
library(tidyverse)

### Jos data

########### load CO2, CH4, N2O sheets - no GWPs applied ########### 

jos_CO2 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                 sheet='CO2',startRow=10)
jos_CO2 <- jos_CO2[1:58]
jos_CO2 <- gather(jos_CO2,year,value,'1970':'2018')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CO2=value)


jos_CH4 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:58]
jos_CH4 <- gather(jos_CH4,year,value,'1970':'2018')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,CH4=value)


jos_N2O <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part A--CO2 CH4 N2O FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:58]
jos_N2O <- gather(jos_N2O,year,value,'1970':'2018')
jos_N2O <- jos_N2O %>% 
  select(ISO,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,N2O=value)


########### Fgas sheets


jos_Fgas <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 Part B--F-gases FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                                  sheet='F-gas (kton)',startRow=10)
jos_Fgas <- jos_Fgas[1:58]
jos_Fgas <- gather(jos_Fgas,year,value,'1970':'2018')
jos_Fgas <- jos_Fgas %>% 
  select(ISO=ISO_A3,year,EDGAR_country=Country,IPCC.detailed=IPCC,IPCC_detailed_description=IPCC_source,Fgas,Fgas_value=value)

jos_Fgas <- spread(jos_Fgas,Fgas,Fgas_value)


########### join sheets ########### 

edgar_GHG <- full_join(jos_CO2,jos_CH4)
edgar_GHG <- full_join(edgar_GHG,jos_N2O)
edgar_GHG <- full_join(edgar_GHG,jos_Fgas)

########### import corrected landfill (6A1) data ###########

landfill <- openxlsx::read.xlsx("Data/EDGAR/updated EDGAR landfills emissions.xlsx","CH4 emi landfills",startRow=2) %>% 
  select(ISO=Country_code_A3,everything(),-emi_id,-Substance)

landfill <- gather(landfill,year,CH4,-ISO)
landfill$year <- gsub("Y_","",landfill$year)
landfill <- landfill %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(CH4=as.numeric(CH4)) %>% 
  mutate(IPCC.detailed="6A1") %>% 
  mutate(IPCC_detailed_description="Managed waste disposal on land")
# 
# landfill <- landfill %>% 
#   mutate(CH4=CH4*1000)

names <- edgar_GHG %>% 
  select(ISO,EDGAR_country,CO2:SF6,-CH4) %>% 
  mutate_at(vars(CO2:SF6),funs(.+NA)) %>% 
  distinct()

landfill <- left_join(landfill,names,by=c("ISO"="ISO")) 
landfill <- landfill %>% 
  select(ISO,EDGAR_country,year,IPCC.detailed,IPCC_detailed_description,CO2,CH4,everything())

edgar_GHG <- edgar_GHG %>% 
  filter(IPCC.detailed!="6A1")

edgar_GHG <- rbind(edgar_GHG,landfill)

rm(jos_CH4,jos_CO2,jos_Fgas,jos_N2O,names,landfill)


########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_sectors.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_sectors %>% select(code,description,IPCC_AR6_chapter,IPCC_AR6_chapter_title,subsector,subsector_title),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,chapter_title=IPCC_AR6_chapter_title,description,subsector,subsector_title,year,everything(),-IPCC_detailed_description)

missing_codes <- edgar_GHG %>% 
  filter(is.na(chapter) | is.na(description) | is.na(sector_code)) %>% 
  distinct()


########### join country names ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

## identify additional countries in EDGAR

missing_iso <- edgar_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO) %>% 
  unique()

### move these countries to the name list 
edgar_GHG <- edgar_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)

########### join region categorisation from WGIII TSU ###########

load('Data/ipcc_regions.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_regions %>% select(-name),by=c("ISO"="ISO"))

missing_region <- edgar_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_5_short)) %>%
  select(ISO,EDGAR_country) %>% 
  unique()

edgar_GHG <- edgar_GHG %>% 
  mutate(region_ar6_5 = as.character(region_ar6_5)) %>% 
  mutate(region_ar6_10 = as.character(region_ar6_10)) %>% 
  mutate(region_ar6_22 = as.character(region_ar6_22)) %>% 
  mutate(region_ar6_dev = as.character(region_ar6_dev))

edgar_GHG$region_ar6_5[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"

edgar_GHG$region_ar6_5[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"

edgar_GHG$region_ar6_5_short[edgar_GHG$ISO=="SEA"] <- "SEA"
edgar_GHG$region_ar6_5_short[edgar_GHG$ISO=="AIR"] <- "AIR"


############## tidying up 

edgar_GHG <- edgar_GHG %>% 
  mutate(year=as.numeric(year)) %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_5_short,region_ar6_10,region_ar6_22,region_ar6_dev,year,chapter,chapter_title,sector_code,description,subsector,subsector_title,CO2,CH4,N2O,everything(),-EDGAR_country)


############## factorise regions


edgar_GHG$region_ar6_5_short <- as.factor(edgar_GHG$region_ar6_5_short)
edgar_GHG$region_ar6_5_short <- factor(edgar_GHG$region_ar6_5_short,levels(edgar_GHG$region_ar6_5_short)[c(1,7,2,3,4,5,6)])


##############convert from Kton to t

column_rows <- names(edgar_GHG %>% select(-ISO,-country,-region_ar6_5,-region_ar6_5_short,-region_ar6_10,-region_ar6_22,-region_ar6_dev,-year,-chapter,-chapter_title,-sector_code,-description,-subsector,-subsector_title))

edgar_GHG <- edgar_GHG %>% 
  mutate_at(vars(all_of(column_rows)),funs(.*1000))
# 
# for (row in 1:nrow(gwps)) {
#   col <- gwps[row, "gas"]
#   edgar_GHG[col] <- edgar_GHG[col]*1000
# }
# 


############## calculate gwps based on ar6 values

load('Data/gwps.RData')
gwps <- gwps %>% 
  filter(gas!="CH4 (fossil/biogenic)") %>% 
  filter(gas!="CH4 (fugitive)") %>% 
  filter(gas!="CH4")

## apply all gwps except CH4
edgar_GHG_ar6 <- edgar_GHG
for (row in 1:nrow(gwps)) {
  col <- gwps[row, "gas"]
  gwp  <- gwps[row, "gwp_ar6"]
  edgar_GHG_ar6[col] <- edgar_GHG_ar6[col]*gwp
}

## apply CH4 gwps based on a more detailed breakdown of sources

edgar_GHG_ar6 <- left_join(edgar_GHG_ar6,gwps_ch4 %>% select(sector_code,gwp_ch4=value),
                           by = "sector_code")
  
edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  mutate(CH4=ifelse(!is.na(gwp_ch4),CH4*gwp_ch4,CH4))

## any missing?
missing_ch4 <- edgar_GHG_ar6 %>% 
  filter(!is.na(CH4) & is.na(gwp_ch4))

## merge all Fgases into a single variable

fgas_list <- gwps %>% 
  filter(gas!="CO2") %>% 
  filter(gas!="N2O") %>% 
  filter(gas!="CH4") %>% 
  select(gas)

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  mutate(Fgas=rowSums(.[fgas_list$gas],na.rm=T)) %>% 
  mutate(nnums=rowSums(!is.na(.[fgas_list$gas]))) %>% 
  mutate(Fgas=ifelse(nnums==0,NA,Fgas)) %>% 
  select(-nnums,-one_of(fgas_list$gas))

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  group_by(ISO,year,sector_code) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))


############## save two datasets

save(edgar_GHG,file='Data/edgar_data_all.RData')
save(edgar_GHG_ar6,file='Data/edgar_data_gwp_ar6.RData')
