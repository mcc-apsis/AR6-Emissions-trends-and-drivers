
rm(list = ls())
library(tidyverse)
library(openxlsx)
library(patchwork)

#load('Data/edgar_data_gwp_ar6.RData')

########### load up EDGAR version 6

edgar_GHG <- openxlsx::read.xlsx('Data/EDGAR/EDGAR_v6.0_emissions_GHG_1970_2018_IPCC contribution_2021.xlsx',
                                 sheet='EDGAR emissions',startRow=5)
edgar_GHG <- gather(edgar_GHG,year,value,Y_1970:Y_2018)

edgar_GHG$year <- gsub("Y_","",edgar_GHG$year)
edgar_GHG$value <- gsub("NULL",NA,edgar_GHG$value)
edgar_GHG <- edgar_GHG %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(value=as.numeric(value))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=Country_code_A3,year,sector_code=IPCC_code_1996,description=IPCC_code_description,fossil_bio,gas=Substance,value)


########### Remove CO2 short cycle biogenic (make another file for this later?)

edgar_GHG <- edgar_GHG %>% 
  mutate(value=ifelse(fossil_bio=="bio" & gas=="CO2",NA,value))


########### merge doubled codes

#1B2b5, bio

edgar_GHG <- edgar_GHG %>% 
  mutate(description=ifelse(sector_code=="1B2b5" & description=="Fuel transformation in Blending natural gas",
                            "Fuel transformation of gaseous fuels (GTL, Blend, (re-)gasif./Liquef., NSF)",description))

#Ethylene Dichloride and Vinyl Chloride production (note: this means we use only Vinyl Chloride production from EDGAR v5FT to project Ethylene Dichloride and Vinyl Chloride for 2019 in EDGAR v6)
edgar_GHG <- edgar_GHG %>% 
  mutate(description=ifelse(sector_code=="2B5g2","Ethylene Dichloride and Vinyl Chloride production",description))

edgar_GHG <- edgar_GHG %>% 
  group_by(ISO,year,sector_code,description,fossil_bio,gas) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  ungroup()

####################### load up fgas replacement

edgar_fgas <- openxlsx::read.xlsx('Data/EDGAR/EDGAR_emissions_v60_F-gases_1990_2018_ipcc_detailed.xlsx')


edgar_fgas <- gather(edgar_fgas,year,value,Y_1990:Y_2018)    ###### !!!!!!!!!!!! CHANGE TO 1970 WHEN EDGAR SUPPLIES IT

edgar_fgas$year <- gsub("Y_","",edgar_fgas$year)
edgar_fgas$value <- gsub("NULL",NA,edgar_fgas$value)
edgar_fgas <- edgar_fgas %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(value=as.numeric(value))

edgar_fgas <- edgar_fgas %>% 
  select(ISO=Country_code_A3,year,sector_code=IPCC_1996,description=IPCC_for_std_report_desc,fossil_bio,gas=Substance,value)

edgar_fgas <- edgar_fgas %>%
  mutate(gas=ifelse(gas=="HFC-43-10mee","HFC-43-10-mee",gas))


#################### substitute fgas in EDGAR v6

edgar_GHG <- edgar_GHG %>% 
  filter(gas %in% c("CO2","CH4","N2O"))

edgar_GHG <- rbind(edgar_GHG,edgar_fgas)


### change another doubled code - f-gas as solvent

edgar_GHG <- edgar_GHG %>% 
  mutate(description=ifelse(sector_code=="2F5","F-gas as Solvent",description))

### add better description to 6Ca

edgar_GHG <- edgar_GHG %>% 
  mutate(description=ifelse(sector_code=="6Ca","Waste incineration - biogenic",description))

blarg <- edgar_GHG %>% ungroup() %>% select(sector_code,description,fossil_bio) %>% distinct()


################## HERE WE SHOULD LOAD UP EDGAR V5 FT; HARMONIZE THE SECTORS WITH V6 AND USE IT TO PROJECT 2019 VALUES
############### SEE import_edgar6_data.R
############### SOME SECTORS / COUNTRIES WILL NOT HAVE GROWTH RATES, SO WE ASSUME CONSTANT GROWTH

########### load Jos fast track data for 2019

jos_CO2 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part A- CO2 (by JRC).xlsx',
                               sheet='CO2',startRow=10)
jos_CO2 <- gather(jos_CO2,year,value,'1970':'2019')
jos_CO2 <- jos_CO2 %>% 
  select(ISO,year,EDGAR_country=Country,sector_code=IPCC_for_std_report_detailed,EDGAR_description=IPCC_source_detailed_desc,CO2=value)


jos_CH4 <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part B- CH4 and N2O (by PBL).xlsx',
                               sheet='CH4',startRow=10)
jos_CH4 <- jos_CH4[1:59]
jos_CH4 <- gather(jos_CH4,year,value,'1970':'2019')
jos_CH4 <- jos_CH4 %>% 
  select(ISO,year,EDGAR_country=Country,sector_code=IPCC,EDGAR_description=IPCC_source,CH4=value)


jos_N2O <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part B- CH4 and N2O (by PBL).xlsx',
                               sheet='N2O',startRow=10)
jos_N2O <- jos_N2O[1:59]
jos_N2O <- gather(jos_N2O,year,value,'1970':'2019')
jos_N2O <- jos_N2O %>% 
  select(ISO,year,EDGAR_country=Country,sector_code=IPCC,EDGAR_description=IPCC_source,N2O=value)
jos_N2O <- jos_N2O %>% 
  mutate(N2O=as.numeric(N2O))


jos_Fgas <- openxlsx::read.xlsx('Data/EDGAR/EDGAR v5.0 FT2019, Part C- F-gases (by PBL).xlsx',
                                sheet='Fgas',startRow=10)
jos_Fgas <- jos_Fgas[1:59]
jos_Fgas <- gather(jos_Fgas,year,value,'1970':'2019')
jos_Fgas <- jos_Fgas %>% 
  select(ISO=ISO_A3,year,EDGAR_country=Country,sector_code=IPCC,EDGAR_description=IPCC_source,Fgas,Fgas_value=value)

jos_Fgas <- spread(jos_Fgas,Fgas,Fgas_value)


########### join Jos sheets ########### 

jos_GHG <- full_join(jos_CO2,jos_CH4)
jos_GHG <- full_join(jos_GHG,jos_N2O)
jos_GHG <- full_join(jos_GHG,jos_Fgas)
rm(jos_CO2,jos_CH4,jos_N2O,jos_Fgas)


########### use edgar v5 GT 2018-2019 trend to project 2019 values in edgar v6

# trim down edgar v5 to the last 2 years
jos_GHG <- gather(jos_GHG %>% filter(year %in% c(2018,2019)),gas,value,-ISO,-year,-EDGAR_country,-sector_code,-EDGAR_description) %>% 
  filter(value!=0) %>% 
  mutate(year=as.numeric(year))

# calculate growth rate by country, code, year and gas

jos_GHG <- jos_GHG %>% 
  group_by(ISO,sector_code,gas) %>% 
  mutate(growth = last(value)/first(value))

# match edgar v5 to v6 codes
matching_codes <- read.xlsx('Data/Codes and classifications/edgar_v5_v6_sector_codes.xlsx',sheet="matched_codes")
jos_GHG <- left_join(jos_GHG,matching_codes %>%
                       select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                     by = c("sector_code"="sector_code_v5"))

not_matched <- anti_join(jos_GHG,matching_codes %>%
                           select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                         by = c("sector_code"="sector_code_v5"))

jos_GHG <- jos_GHG %>% 
  ungroup() %>% 
  select(ISO,year,sector_code=sector_code_v6,description=description_v6,fossil_bio,gas,growth)


# join to edgar v6
edgar_GHG_2018 <- left_join(edgar_GHG %>% filter(year==2018),jos_GHG,by = c("ISO","year","sector_code","description","fossil_bio","gas"))
not_joined <- anti_join(edgar_GHG %>% filter(year==2018),jos_GHG,by = c("ISO","year","sector_code","description","fossil_bio","gas"))

# if we have no growth rates in a row, assume constant level of emissions, but save this information
edgar_GHG_2019 <- edgar_GHG_2018 %>% 
  mutate(projection = ifelse(!is.na(value) & is.na(growth),"stable (no data)","projected")) %>% 
  mutate(growth=ifelse(is.na(growth),1,growth)) %>%
  mutate(`2019`=value*growth)
edgar_GHG_2019 <- edgar_GHG_2019 %>% 
  select(-year,-value)
edgar_GHG_2019 <- gather(edgar_GHG_2019,year,value,`2019`) %>% 
  select(-growth)

edgar_GHG <- rbind(edgar_GHG %>% mutate(projection=NA),edgar_GHG_2019)

#################################################

########### join ipcc sector mapping  ########### 

load('Data/ipcc_sectors.RData')
#sector_codes <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDGARv6_FGD.xlsx',sheet=2)


edgar_GHG <- left_join(edgar_GHG,ipcc_sectors %>% select(code,fossil_bio,chapter=IPCC_AR6_chapter,chapter_title=IPCC_AR6_chapter_title,
                                                         subsector,subsector_title),by=c("sector_code"="code","fossil_bio"="fossil_bio"))

missing_codes <- edgar_GHG %>% 
  filter(is.na(subsector)) %>% 
  select(sector_code,fossil_bio,description) %>% 
  distinct()  #no missing codes left



########### join country names with international ISO standard ########### 

#country_codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
country_codes <- openxlsx::read.xlsx('Data/Codes and classifications/ISOcodes.xlsx',sheet = 'ISO_master')

edgar_country_names <- openxlsx::read.xlsx('Data/EDGAR/EDGAR_v6.0_emissions_GHG_1970_2018_IPCC contribution_2021.xlsx',
                                           sheet=2)

edgar_country_ISOs <- edgar_GHG %>% select(ISO) %>% distinct()
edgar_country_ISOs <- left_join(edgar_country_ISOs,edgar_country_names,by=c("ISO"="Country.ISO.code"))
edgar_country_ISOs <- left_join(edgar_country_ISOs,country_codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

## add additional countries in EDGAR to the name list (e.g. shipping)

edgar_country_ISOs <- edgar_country_ISOs %>% 
  mutate(name=ifelse(is.na(name),Country.name,name))

edgar_GHG <- left_join(edgar_GHG,edgar_country_ISOs %>% select(-Country.name),by=c("ISO"))

# any missing countries?
missing_countries <- edgar_GHG %>% filter(is.na(name))


########### join region categorisation from WGIII TSU ###########

load('Data/ipcc_regions.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_regions %>% select(-name),by=c("ISO"="ISO"))

missing_region <- edgar_GHG %>% 
  filter(is.na(region_ar6_6) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_6_short) | is.na(region_ar6_10_short)) %>%
  select(ISO,name) %>% 
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


projected <- edgar_GHG %>% 
  filter(year==2019) %>% 
  select(ISO,sector_code,fossil_bio,gas,value,projection,year)

edgar_GHG <- edgar_GHG %>% 
  mutate(year=as.numeric(year)) %>% 
  select(ISO,country=name,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year,chapter,chapter_title,sector_code,fossil_bio,description,subsector,subsector_title,gas,value)

############## factorise regions

edgar_GHG$region_ar6_6_short <- as.factor(edgar_GHG$region_ar6_6_short)
edgar_GHG$region_ar6_6_short <- factor(edgar_GHG$region_ar6_6_short,levels(edgar_GHG$region_ar6_6_short)[c(2,8,1,3,4,5,6,7)])


##############convert from Kton to t

edgar_GHG <- edgar_GHG %>% 
  mutate(value=value*1000)

############## join gwps and calculate GHG emissions, using ar5 and ar6 values

load('Data/gwps.RData')
gwps <- gwps %>% 
  filter(gas!="CH4")

edgar_GHG <- left_join(edgar_GHG,gwps %>% select(gas,gwp_ar6,gwp_ar5,gwp_ar5_feedbacks,gwp_ar4,gwp_ar2),by="gas")

# any gases now missing ?
missing_gases_ar6 <- anti_join(gwps %>% select(gas,gwp_ar6),edgar_GHG,by="gas")
missing_gases_ar5_feedbacks <- anti_join(gwps %>% select(gas,gwp_ar5_feedbacks),edgar_GHG,by="gas")
missing_gases_ar5 <- anti_join(gwps %>% select(gas,gwp_ar5),edgar_GHG,by="gas")
missing_gases_ar4 <- anti_join(gwps %>% select(gas,gwp_ar4),edgar_GHG,by="gas")
missing_gases_ar2 <- anti_join(gwps %>% select(gas,gwp_ar2),edgar_GHG,by="gas")

# do we have all the non-CH4 gases?

missing_gwps_ar6 <- edgar_GHG %>% filter(is.na(gwp_ar6)) %>% select(sector_code,gas) %>% distinct()
missing_gwps_ar5_feedbacks <- edgar_GHG %>% filter(is.na(gwp_ar5_feedbacks)) %>% select(sector_code,gas) %>% distinct()
missing_gwps_ar5 <- edgar_GHG %>% filter(is.na(gwp_ar5)) %>% select(sector_code,gas) %>% distinct()
missing_gwps_ar4 <- edgar_GHG %>% filter(is.na(gwp_ar4)) %>% select(sector_code,gas) %>% distinct()
missing_gwps_ar2 <- edgar_GHG %>% filter(is.na(gwp_ar2)) %>% select(sector_code,gas) %>% distinct()

## get CH4 gwps based on a more detailed breakdown of sources

gwps_ch4 <- gwps_ch4 %>% select(sector_code,fossil_bio,ch4_gwp_ar6=gwp_ar6,ch4_gwp_ar5=gwp_ar5,ch4_gwp_ar5_fb=gwp_ar5_feedbacks,ch4_gwp_ar4=gwp_ar4,ch4_gwp_ar2=gwp_ar2)
edgar_GHG <- left_join(edgar_GHG,gwps_ch4,by = c("sector_code","fossil_bio"))

# do we have all the CH4 gases?

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
  select(-ch4_gwp_ar6,-ch4_gwp_ar5,-ch4_gwp_ar2,-ch4_gwp_ar4,-ch4_gwp_ar5_fb)

## apply all gwps
edgar_GHG_ar6 <- edgar_GHG %>% mutate(value_gwp=value*gwp_ar6)
edgar_GHG_ar5 <- edgar_GHG %>% mutate(value_gwp=value*gwp_ar5)

## merge all Fgases into a single variable

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% select(-value,-gwp_ar6,-gwp_ar5,-gwp_ar4,-gwp_ar2,-gwp_ar5_feedbacks)
edgar_GHG_ar5 <- edgar_GHG_ar5 %>% select(-value,-gwp_ar6,-gwp_ar5,-gwp_ar4,-gwp_ar2,-gwp_ar5_feedbacks)

edgar_GHG_ar6 <- spread(edgar_GHG_ar6,gas,value_gwp)
edgar_GHG_ar5 <- spread(edgar_GHG_ar5,gas,value_gwp)

fgas_list_ar6 <- names(edgar_GHG_ar6[-(1:16)] %>% select(-CO2,-CH4,-N2O))
fgas_list_ar5 <- names(edgar_GHG_ar5[-(1:16)] %>% select(-CO2,-CH4,-N2O))

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


## save both files

edgar_raw <- edgar_GHG
save(edgar_raw,file='Data/edgar6_v2_data_raw.RData')

edgar_ghg <- edgar_GHG_ar6
save(edgar_ghg,file='Data/edgar6_v2_data_ghg_gwp_ar6.RData')

edgar_ghg <- edgar_GHG_ar5
save(edgar_ghg,file='Data/edgar6_v2_data_ghg_gwp_ar5.RData')


#### how much of 2019 is projected vs stable?

projected <- left_join(projected,gwps %>% select(gas,gwp_ar6))
projected <- projected %>% 
  mutate(gwp_ar6=ifelse(gas=="CH4",29,gwp_ar6)) %>% 
  mutate(value_gwp=value*gwp_ar6)

blarg <- projected %>% 
  group_by(projection) %>% 
  summarise(value_gwp=sum(value_gwp,na.rm=TRUE))
