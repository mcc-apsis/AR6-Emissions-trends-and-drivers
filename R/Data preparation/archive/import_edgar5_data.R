
rm(list = ls())
library(tidyverse)

### Jos data

########### load CO2, CH4, N2O sheets - no GWPs applied ########### 

jos_CO2 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part A- CO2 (by JRC).xlsx',
                                 sheet='CO2',startRow=10)
jos_CO2 <- gather(jos_CO2,year,value,'1970':'2019')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,year,EDGAR_country=Country,code=IPCC_for_std_report_detailed,EDGAR_description=IPCC_source_detailed_desc,CO2=value)


jos_CH4 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part B- CH4 and N2O (by PBL).xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:59]
jos_CH4 <- gather(jos_CH4,year,value,'1970':'2019')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,year,EDGAR_country=Country,code=IPCC,EDGAR_description=IPCC_source,CH4=value)


jos_N2O <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part B- CH4 and N2O (by PBL).xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:59]
jos_N2O <- gather(jos_N2O,year,value,'1970':'2019')
jos_N2O <- jos_N2O %>% 
  select(ISO,year,EDGAR_country=Country,code=IPCC,EDGAR_description=IPCC_source,N2O=value)
jos_N2O <- jos_N2O %>% 
  mutate(N2O=as.numeric(N2O))

########### Fgas sheets


jos_Fgas <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part C- F-gases (by PBL).xlsx',
                                  sheet='Fgas',startRow=10)
jos_Fgas <- jos_Fgas[1:59]
jos_Fgas <- gather(jos_Fgas,year,value,'1970':'2019')
jos_Fgas <- jos_Fgas %>% 
  select(ISO=ISO_A3,year,EDGAR_country=Country,code=IPCC,EDGAR_description=IPCC_source,Fgas,Fgas_value=value)

jos_Fgas <- spread(jos_Fgas,Fgas,Fgas_value)


########### join sheets ########### 

edgar_GHG <- full_join(jos_CO2,jos_CH4)
edgar_GHG <- full_join(edgar_GHG,jos_N2O)
edgar_GHG <- full_join(edgar_GHG,jos_Fgas)

########### import corrected landfill (6A1) data ###########

# landfill <- openxlsx::read.xlsx("Data/EDGAR/updated EDGAR landfills emissions.xlsx","CH4 emi landfills",startRow=2) %>% 
#   select(ISO=Country_code_A3,everything(),-emi_id,-Substance)
# 
# landfill <- gather(landfill,year,CH4,-ISO)
# landfill$year <- gsub("Y_","",landfill$year)
# landfill <- landfill %>% 
#   mutate(year=as.numeric(year)) %>% 
#   mutate(CH4=as.numeric(CH4)) %>% 
#   mutate(IPCC.detailed="6A1") %>% 
#   mutate(IPCC_detailed_description="Managed waste disposal on land")
# 
# names <- edgar_GHG %>% 
#   select(ISO,EDGAR_country,CO2:SF6,-CH4) %>% 
#   mutate_at(vars(CO2:SF6),funs(.+NA)) %>% 
#   distinct()
# 
# landfill <- left_join(landfill,names,by=c("ISO"="ISO")) 
# landfill <- landfill %>% 
#   select(ISO,EDGAR_country,year,IPCC.detailed,IPCC_detailed_description,CO2,CH4,everything())
# 
# edgar_GHG <- edgar_GHG %>% 
#   filter(IPCC.detailed!="6A1")
# 
# edgar_GHG <- rbind(edgar_GHG,landfill)
# 
# rm(jos_CH4,jos_CO2,jos_Fgas,jos_N2O,names,landfill)


########### join source categories from compilation (see category_mapping.R) ########### 

# match edgar v5 to v6 codes
matching_codes <- read.xlsx('Data/Codes and classifications/edgar_v5_v6_sector_codes.xlsx',sheet="matched_codes")
edgar_GHG <- left_join(edgar_GHG,matching_codes %>%
                       select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                     by = c("code"="sector_code_v5"))

not_matched <- anti_join(edgar_GHG,matching_codes %>%
                           select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                         by = c("code"="sector_code_v5"))

# edgar 6 has two descriptions for 2B5g2, which doubles these codes in edgar 5. remove one of them until they send us a clean sheet
edgar_GHG <- edgar_GHG %>%
  filter(description_v6!="Ethylene chloride production")


load('Data/ipcc_sectors.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_sectors %>% select(code,fossil_bio,IPCC_AR6_chapter,IPCC_AR6_chapter_title,subsector,subsector_title),by=c("sector_code_v6"="code","fossil_bio"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO,sector_code=sector_code_v6,sector_code_v5=code,chapter=IPCC_AR6_chapter,chapter_title=IPCC_AR6_chapter_title,description_v5=EDGAR_description,description=description_v6,subsector,subsector_title,year,everything())

missing_codes <- edgar_GHG %>% 
  filter(is.na(chapter) | is.na(description) | is.na(sector_code)) %>% 
  distinct()


########### join country names ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

## identify additional countries in EDGAR

missing_iso <- edgar_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO,EDGAR_country) %>% 
  unique()

### move these countries to the name list 
edgar_GHG <- edgar_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)

########### join region categorisation from WGIII TSU ###########

load('Data/ipcc_regions.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_regions %>% select(-name),by=c("ISO"="ISO"))

missing_region <- edgar_GHG %>% 
  filter(is.na(region_ar6_6) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_6_short) | is.na(region_ar6_10_short)) %>%
  select(ISO,EDGAR_country) %>% 
  unique()

edgar_GHG <- edgar_GHG %>% 
  mutate(region_ar6_6 = as.character(region_ar6_6)) %>% 
  mutate(region_ar6_10 = as.character(region_ar6_10)) %>% 
  mutate(region_ar6_22 = as.character(region_ar6_22)) %>% 
  mutate(region_ar6_dev = as.character(region_ar6_dev))

edgar_GHG$region_ar6_6[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="AIR"] <- "Intl. Aviation"

edgar_GHG$region_ar6_6[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_10[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_22[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"
edgar_GHG$region_ar6_dev[edgar_GHG$ISO=="SEA"] <- "Intl. Shipping"

edgar_GHG$region_ar6_6_short[edgar_GHG$ISO=="SEA"] <- "SEA"
edgar_GHG$region_ar6_6_short[edgar_GHG$ISO=="AIR"] <- "AIR"

edgar_GHG$region_ar6_10_short[edgar_GHG$ISO=="SEA"] <- "SEA"
edgar_GHG$region_ar6_10_short[edgar_GHG$ISO=="AIR"] <- "AIR"

############## tidying up 

edgar_GHG <- edgar_GHG %>% 
  mutate(year=as.numeric(year)) %>% 
  select(ISO,country=name,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year,chapter,chapter_title,subsector,subsector_title,sector_code,sector_code_v5,fossil_bio,description,description_v5,subsector,subsector_title,CO2,CH4,N2O,everything(),-EDGAR_country)


############## factorise regions


edgar_GHG$region_ar6_6_short <- as.factor(edgar_GHG$region_ar6_6_short)
edgar_GHG$region_ar6_6_short <- factor(edgar_GHG$region_ar6_6_short,levels(edgar_GHG$region_ar6_6_short)[c(2,8,1,3,4,5,6,7)])


############## gather and remove zero values (file is a lot easier to handle and edgar v6 mostly did this)

gases <- names(edgar_GHG[-(1:18)])
edgar_GHG <- gather(edgar_GHG,gas,value,gases)
edgar_GHG <- edgar_GHG %>% 
  filter(value!=0)

############## convert from Kton to t

edgar_GHG <- edgar_GHG %>% 
  mutate(value=value*1000)

############## calculate gwps based on ar6 values

load('Data/gwps.RData')
gwps <- gwps %>% 
  filter(gas!="CH4")

edgar_GHG <- left_join(edgar_GHG,gwps %>% select(gas,gwp_ar6,gwp_ar5,gwp_ar5_feedbacks,gwp_ar4,gwp_ar2),by="gas")

# any gwps now missing ?
missing_gwps_ar6 <- anti_join(gwps %>% select(gas,gwp_ar6),edgar_GHG,by="gas")
missing_gwps_ar5_feedbacks <- anti_join(gwps %>% select(gas,gwp_ar5_feedbacks),edgar_GHG,by="gas")
missing_gwps_ar5 <- anti_join(gwps %>% select(gas,gwp_ar5),edgar_GHG,by="gas")
missing_gwps_ar4 <- anti_join(gwps %>% select(gas,gwp_ar4),edgar_GHG,by="gas")
missing_gwps_ar2 <- anti_join(gwps %>% select(gas,gwp_ar2),edgar_GHG,by="gas")

## get CH4 gwps based on a more detailed breakdown of sources

gwps_ch4 <- gwps_ch4 %>% select(sector_code,fossil_bio,ch4_gwp_ar6=gwp_ar6,ch4_gwp_ar5=gwp_ar5,ch4_gwp_ar5_fb=gwp_ar5_feedbacks,ch4_gwp_ar4=gwp_ar4,
                                ch4_gwp_ar2=gwp_ar2)

edgar_GHG <- left_join(edgar_GHG,gwps_ch4,by = c("sector_code","fossil_bio"))

# do we have all the non-CH4 gases?

ch4_gwps <- edgar_GHG %>% 
  filter(gas=="CH4") %>% 
  select(sector_code,fossil_bio,description,gas,ch4_gwp_ar6,ch4_gwp_ar5,ch4_gwp_ar5_fb,ch4_gwp_ar4,ch4_gwp_ar2) %>% 
  distinct()

edgar_GHG <- edgar_GHG %>% 
  mutate(gwp_ar6=ifelse(gas=="CH4",ch4_gwp_ar6,gwp_ar6)) %>% 
  mutate(gwp_ar5_feedbacks=ifelse(gas=="CH4",ch4_gwp_ar5_fb,gwp_ar5_feedbacks)) %>% 
  mutate(gwp_ar5=ifelse(gas=="CH4",ch4_gwp_ar5,gwp_ar5)) %>% 
  mutate(gwp_ar4=ifelse(gas=="CH4",ch4_gwp_ar4,gwp_ar4)) %>% 
  mutate(gwp_ar2=ifelse(gas=="CH4",ch4_gwp_ar2,gwp_ar2)) %>% 
  select(-ch4_gwp_ar6,-ch4_gwp_ar5,-ch4_gwp_ar4,-ch4_gwp_ar2,-ch4_gwp_ar5_fb)

## apply all gwps
edgar_GHG_ar6 <- edgar_GHG %>% mutate(value_gwp=value*gwp_ar6)
edgar_GHG_ar5 <- edgar_GHG %>% mutate(value_gwp=value*gwp_ar5)

## merge all Fgases into a single variable

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% select(-value,-gwp_ar6,-gwp_ar5,-gwp_ar4,-gwp_ar2,-gwp_ar5_feedbacks)
edgar_GHG_ar5 <- edgar_GHG_ar5 %>% select(-value,-gwp_ar6,-gwp_ar5,-gwp_ar4,-gwp_ar2,-gwp_ar5_feedbacks)

edgar_GHG_ar6 <- spread(edgar_GHG_ar6,gas,value_gwp)
edgar_GHG_ar5 <- spread(edgar_GHG_ar5,gas,value_gwp)

fgas_list_ar6 <- names(edgar_GHG_ar6[-(1:18)] %>% select(-CO2,-CH4,-N2O))
fgas_list_ar5 <- names(edgar_GHG_ar5[-(1:18)] %>% select(-CO2,-CH4,-N2O))

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  mutate(Fgas=rowSums(.[fgas_list_ar6],na.rm=T)) %>% 
  mutate(Fgas=ifelse(Fgas==0,NA,Fgas))

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  mutate(Fgas=rowSums(.[fgas_list_ar5],na.rm=T)) %>% 
  mutate(Fgas=ifelse(Fgas==0,NA,Fgas))

## remove underlying fgases

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% select(-one_of(fgas_list_ar6))
edgar_GHG_ar5 <- edgar_GHG_ar5 %>% select(-one_of(fgas_list_ar5))


## calculate total GHG emissions

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  group_by(ISO,year,sector_code,fossil_bio) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  group_by(ISO,year,sector_code,fossil_bio) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

## rename gwps for authors

edgar_GHG <- edgar_GHG %>% 
  select(everything(),gwp100_ar6=gwp_ar6,gwp100_ar5=gwp_ar5,gwp100_ar5_fb=gwp_ar5_feedbacks,gwp100_ar4=gwp_ar4,gwp100_ar2=gwp_ar2) %>%  
  relocate(value,.after=gwp100_ar2)

## relevel the gases

edgar_GHG$gas <- as.factor(edgar_GHG$gas) 
edgar_GHG$gas <- fct_relevel(edgar_GHG$gas,c("CO2","CH4","N2O"))


############## save two datasets

edgar_raw <- edgar_GHG
save(edgar_raw,file='Data/edgar5_data_raw.RData')

edgar_ghg <- edgar_GHG_ar6
save(edgar_ghg,file='Data/edgar5_data_ghg_gwp_ar6.RData')

edgar_ghg <- edgar_GHG_ar5
save(edgar_ghg,file='Data/edgar5_data_ghg_gwp_ar5.RData')
