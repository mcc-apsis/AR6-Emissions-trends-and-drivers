
rm(list = ls())
library(tidyverse)
library(openxlsx)

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


# blarg <- edgar_GHG %>% 
#   filter(gas=="CH4") %>% 
#   group_by(year) %>% 
#   summarise(value=sum(value,na.rm=T))
# 
# blarg %>% ggplot(.,aes(x=year,y=value)) +
#   geom_path()

########### Remove CO2 short cycle biogenic


edgar_GHG <- edgar_GHG %>% 
  mutate(value=ifelse(fossil_bio=="bio" & gas=="CO2",NA,value))


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

#jos_GHG <- gather(jos_GHG,gas,value,-ISO,-year,-EDGAR_country,-code,-EDGAR_description) %>% 
#  filter(year==2019)


# remove zeros in edgar v5 and filter to 2019
jos_GHG <- gather(jos_GHG,gas,value,-ISO,-year,-EDGAR_country,-sector_code,-EDGAR_description) %>% 
  filter(value!=0) %>% 
  filter(year==2019) %>% 
  mutate(year=as.numeric(year))

# match edgar v5 to v6 codes
matching_codes <- read.xlsx('Data/Codes and classifications/edgar_v5_v6_sector_codes.xlsx',sheet="matched_codes")
jos_GHG <- left_join(jos_GHG,matching_codes %>% 
                       select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                     by = c("sector_code"="sector_code_v5"))

not_matched <- anti_join(jos_GHG,matching_codes %>% 
                           select(sector_code_v5,sector_code_v6,fossil_bio,description_v6),
                         by = c("sector_code"="sector_code_v5"))

# fix the vinyl / ethylene chloride problem
jos_GHG <- jos_GHG %>% 
  filter(description_v6!="Ethylene chloride production")

# match up columns to edgar v6
jos_GHG <- jos_GHG %>% 
  select(ISO,year,sector_code=sector_code_v6,description=description_v6,fossil_bio,gas,value)

### add edgar v5 to v6

edgar_GHG <- rbind(edgar_GHG,jos_GHG)


########### join ipcc sector mapping  ########### 

#load('Data/ipcc_sectors.RData')
sector_codes <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDGARv6_FGD.xlsx',sheet=2)


edgar_GHG <- left_join(edgar_GHG,sector_codes %>% select(code,fossil_bio,chapter=IPCC_AR6_chapter,chapter_title=IPCC_AR6_chapter_title,
                                            subsector,subsector_title),by=c("sector_code"="code","fossil_bio"="fossil_bio"))

missing_codes <- edgar_GHG %>% 
  filter(is.na(chapter)) %>% 
  select(sector_code,fossil_bio,description,gas) %>% 
  distinct()


########### join country names with international ISO standard ########### 

country_codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')

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
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev) | is.na(region_ar6_5_short)) %>%
  select(ISO,name) %>% 
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
  select(ISO,country=name,region_ar6_5,region_ar6_5_short,region_ar6_10,region_ar6_22,region_ar6_dev,year,chapter,chapter_title,sector_code,fossil_bio,description,subsector,subsector_title,gas,value)

############## factorise regions

edgar_GHG$region_ar6_5_short <- as.factor(edgar_GHG$region_ar6_5_short)
edgar_GHG$region_ar6_5_short <- factor(edgar_GHG$region_ar6_5_short,levels(edgar_GHG$region_ar6_5_short)[c(2,8,1,3,4,5,6,7)])


##############convert from Kton to t

edgar_GHG <- edgar_GHG %>% 
  mutate(value=value*1000)

############## calculate gwps based on ar6 values

load('Data/gwps.RData')
gwps <- gwps %>% 
  filter(gas!="CH4")

edgar_GHG <- left_join(edgar_GHG,gwps %>% select(gas,gwp_ar6),by="gas")

# any gases now missing ?
missing_gases <- anti_join(gwps %>% select(gas,gwp_ar6),edgar_GHG,by="gas")

# do we have all the non-CH4 gases?

missing_gwps <- edgar_GHG %>% 
  filter(is.na(gwp_ar6)) %>% select(sector_code,gas) %>% distinct()

## get CH4 gwps based on a more detailed breakdown of sources

edgar_GHG <- left_join(edgar_GHG,gwps_ch4 %>% select(sector_code,fossil_bio,ch4_gwp_ar6=gwp_ar6),
                           by = c("sector_code","fossil_bio"))

# do we have all the CH4 gases?

ch4_gwps <- edgar_GHG %>% 
  filter(gas=="CH4") %>% 
  #filter(is.na(ch4_gwp_ar6)) %>% 
  select(sector_code,fossil_bio,description,gas,ch4_gwp_ar6) %>% 
  distinct()

edgar_GHG <- edgar_GHG %>% 
  mutate(gwp_ar6=ifelse(gas=="CH4",ch4_gwp_ar6,gwp_ar6)) %>% 
  select(-ch4_gwp_ar6)

## apply all gwps
edgar_GHG_ar6 <- edgar_GHG %>% 
  mutate(value_gwp=value*gwp_ar6)

## merge all Fgases into a single variable

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  select(-value,-gwp_ar6)
edgar_GHG_ar6 <- spread(edgar_GHG_ar6,gas,value_gwp)

fgas_list <- names(edgar_GHG_ar6[-(1:15)] %>% select(-CO2,-CH4,-N2O))

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  mutate(Fgas=rowSums(.[fgas_list],na.rm=T)) %>% 
  mutate(Fgas=ifelse(Fgas==0,NA,Fgas))

## remove underlying fgases

edgar_GHG_ar6 <- edgar_GHG_ar6 %>%  
  select(-one_of(fgas_list))

## calculate total GHG emissions

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  group_by(ISO,year,sector_code,fossil_bio) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

## rename gwps for authors

edgar_GHG <- edgar_GHG %>% 
  select(everything(),gwp100_ar6=gwp_ar6) %>% 
  relocate(value,.after=gwp100_ar6)


############## build a summary sheet of sectors vs gases

summary <- edgar_GHG %>% 
  filter(year==2019) %>% 
  group_by(chapter,chapter_title,subsector_title,sector_code,fossil_bio,description,gas,gwp100_ar6) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  arrange(chapter,sector_code)

summary_gwps <- edgar_GHG_ar6 %>% 
  filter(year==2019) %>% 
  group_by(chapter,chapter_title,subsector_title,sector_code,fossil_bio,description) %>% 
  summarise_at(vars(all_of(c("CO2","CH4","N2O","Fgas","GHG"))),sum,na.rm=TRUE) %>% 
  arrange(chapter,sector_code)


info = data.frame("x" = c("Last update","source"),
                  "y" = c(as.character(Sys.time()),
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_summary")
addWorksheet(wb,"info")
addWorksheet(wb,"sectors_gases_2018")
addWorksheet(wb,"sectors_gases_gwps_2018")
writeData(wb, sheet = "info",info,colNames=F)
writeData(wb, sheet = "sectors_gases_2018",summary,colNames=T)
writeData(wb, sheet = "sectors_gases_gwps_2018",summary_gwps,colNames=T)
saveWorkbook(wb,"Results/Data/ipcc_ar6_data_summary.xlsx",overwrite = T)



## save both files


save(edgar_GHG,file='Data/edgar6_data_all.RData')
save(edgar_GHG_ar6,file='Data/edgar6_data_gwp_ar6.RData')
