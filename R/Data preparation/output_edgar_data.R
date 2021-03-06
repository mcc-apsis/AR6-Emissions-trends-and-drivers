
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar_essd_data_ghg_gwp_ar5.RData')
load('Data/land.RData')
load('Data/ipcc_regions.RData')
load('Data/ipcc_sectors.RData')
load('Data/gwps.RData')
load('Data/WDI_gdp_pop.RData')

#################### prep


edgar_ghg <- gather(edgar_ghg,gas,value,CH4:GHG)
edgar_ghg$gas <- as.factor(edgar_ghg$gas)
edgar_ghg$gas <- fct_relevel(edgar_ghg$gas,"CO2","CH4","N2O","Fgas","GHG")
edgar_ghg <- spread(edgar_ghg,gas,value)

edgar_ghg <- edgar_ghg %>% 
  filter(year>1969) %>%  
  arrange(ISO)

gwps_ch4 <- gwps_ch4 %>% 
  select(-value,-gwp_ar6,-gwp_ar4,-gwp_ar2)

gwps <- gwps %>% 
  select(gas,gwp_ar5)

edgar_ghg <- left_join(edgar_ghg,wdi_data_gdp_pop %>% select(-gdp_real),by=c("ISO"="iso3c","year"))


#################### meta data

meta <- data.frame("Variable" = c("CO2","CH4","N2O","Fgas","GHG","gdp_ppp","population","blue","houghton","oscar","mean"),
                   "Description" = c("Carbon dioxide emissions","Methane emissions","Nitrous oxide emissions","Flourinated gas emissions","Total greenhouse gas emissions","Gross Domestic Product","Population","Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions"),
                   "Units" = c("tCO2","tCO2eq","tCO2eq","tCO2eq","tCO2eq","US $ (constant 2017 international PPP)","persons","tCO2","tCO2","tCO2","tCO2"),
                   "Source" = c("EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0","World Bank","World Bank","BLUE (bookkeeping of land use emissions) model","Houghton & Nassikas bookeeping model","OSCAR, a compact Earth system model","Mean of BLUE, H&N, OSCAR"),
                   "Citation" = c("Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                  "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                  "Hansis, E., Davis, S. J., & Pongratz, J. (2015). Relevance of methodological choices for accounting of land use change carbon fluxes. Global Biogeochemical Cycles, 29(8), 1230–1246. https://doi.org/10.1002/2014GB004997",
                                  "Houghton, R. A., & Nassikas, A. A. (2017). Global and regional fluxes of carbon from land use and land cover change 1850–2015. Global Biogeochemical Cycles, 31, 456–472. https://doi.org/10.1002/2016GB005546",
                                  "Gasser, T., Crepin, L., Quilcaille, Y., Houghton, R. A., Ciais, P., & Obersteiner, M. (2020). Historical CO2 emissions from land use and land cover change and their uncertainty. Biogeosciences, 17(15), 4075–4101. https://doi.org/10.5194/bg-17-4075-2020",
                                  "Hansis et al. 2015, Houghton et al. 2017, Gasser et al. 2020"))


#################### info

info = data.frame("x" = c("Data description","Appropriate use","Global warming potentials","Year coverage","Fossil vs. bio sources","Sector codes","Region codes","Author & contact","R code","Land use data","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "This data is only authorised for use within the IPCC AR6 process, as it contains a level of detail that is not currently publically available. To use this data for non-IPCC purposes, including the publication of articles, please contact Monica Crippa at the Joint Research Centre (Monica.CRIPPA@ec.europa.eu).",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on values from AR6 WGI (see '100_yr_gwps' tab). In AR6 CH4 emissions have two separate GWPs for biogenic and fugitive sources. The specific sources and their CH4 GWPs are listed in the CH4_gwps tab.",
                          "This data file provides annual coverage to 2019 for all emissions sources. We expect a further update in September 2021 to finalise the dataset, in particular the provisional 2019 values.",
                          "In June 2021 we updated to EDGAR version 6. Among other changes, this version adds a new `fossil_bio` column. This indicates whether a particular source is fossil or biogenic in origin. In particular, this matters for distinguishing the global warming potentials for methane sources. Previously this information was captured by an ´x` notation in the sector_code column. Please check your analysis to be sure this has no unintended effects.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "CO2-LULUCF emissions are also available, but for IPCC regions only.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



#################### full file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_edgar_essd_gwp100.xlsx")
addWorksheet(wb,"info")
addWorksheet(wb,"metadata")
addWorksheet(wb,"data")
addWorksheet(wb,"data (CO2-LULUCF)")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")
addWorksheet(wb,"100_yr_gwps")
addWorksheet(wb,"CH4_gwps")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "metadata", meta, colNames = T)
writeData(wb, sheet = "data", edgar_ghg, colNames = T)
writeData(wb, sheet = "data (CO2-LULUCF)",land, colNames = T)
writeData(wb, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_data_edgar_essd_gwp100.xlsx",overwrite = T)


write.csv(x = edgar_ghg,file="Results/Data/test.csv",sep = ",")



########################## NO GWPS

load('Data/edgar_essd_data_raw.RData')

edgar_raw <- edgar_raw %>% 
  select(-gwp100_ar2,-gwp100_ar2,-gwp100_ar6) %>% 
  arrange(ISO)

edgar_raw <- left_join(edgar_raw,wdi_data_gdp_pop %>% select(-gdp_real),by=c("ISO"="iso3c","year"))

info = data.frame("x" = c("Data description","Appropriate use","Global warming potentials","Year coverage","Fossil vs. bio sources","Sector codes","Region codes","Author & contact","R code","Land use data","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "This data is only authorised for use within the IPCC AR6 process, as it contains a level of detail that is not currently publically available. To use this data for non-IPCC purposes, including the publication of articles, please contact Monica Crippa at the Joint Research Centre (Monica.CRIPPA@ec.europa.eu).",
                          "This data file provides a non-aggregated list of greenhouse gases in their original units. For convenience, we also include the 100 year global warming potentials provided by working group I as a column in the dataset. Please see the separate file 'ipcc_ar6_data_edgar_gwp100.xlsx' for data already aggregated to 100 year global warming potentials.",
                          "This data file provides annual coverage to 2019 for all emissions sources. We expect a further update in September 2021 to finalise the dataset, in particular the provisional 2019 values.",
                          "In June 2021 we updated to EDGAR version 6. Among other changes, this version adds a new `fossil_bio` column. This indicates whether a particular source is fossil or biogenic in origin. In particular, this matters for distinguishing the global warming potentials for methane sources. Previously this information was captured by an ´x` notation in the sector_code column. Please check your analysis to be sure this has no unintended effects.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "CO2-LULUCF emissions are also available, but for IPCC regions only.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_raw <- data.frame("Variable" = c("gas","gwp100_ar5","value","gdp_ppp","population","blue","houghton","oscar","mean"),
                   "Description" = c("Greenhouse gas (CO2, CH4, N2O or Fgas)","Global warming potential with a 100 year time horizon, AR6 values","Total value for each sector","Gross Domestic Product","Population","Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions"),
                   "Units" = c("NA","NA","t native units","US $ (constant 2017 international PPP)","persons","tCO2","tCO2","tCO2","tCO2"),
                   "Source" = c("NA","IPCC AR5 WG1","EDGAR_v6.0","World Bank","World Bank","BLUE (bookkeeping of land use emissions) model","Houghton & Nassikas bookeeping model","OSCAR, a compact Earth system model","Mean of BLUE, H&N, OSCAR"),
                   "Citation" = c("NA","IPCC AR5 WG1",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                  "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                  "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                  "Hansis, E., Davis, S. J., & Pongratz, J. (2015). Relevance of methodological choices for accounting of land use change carbon fluxes. Global Biogeochemical Cycles, 29(8), 1230–1246. https://doi.org/10.1002/2014GB004997",
                                  "Houghton, R. A., & Nassikas, A. A. (2017). Global and regional fluxes of carbon from land use and land cover change 1850–2015. Global Biogeochemical Cycles, 31, 456–472. https://doi.org/10.1002/2016GB005546",
                                  "Gasser, T., Crepin, L., Quilcaille, Y., Houghton, R. A., Ciais, P., & Obersteiner, M. (2020). Historical CO2 emissions from land use and land cover change and their uncertainty. Biogeosciences, 17(15), 4075–4101. https://doi.org/10.5194/bg-17-4075-2020",
                                  "Hansis et al. 2015, Houghton et al. 2017, Gasser et al. 2020"))



wb2 <- openxlsx::createWorkbook(title = "ipcc_ar6_data_edgar_essd_nogwps")
addWorksheet(wb2,"info")
addWorksheet(wb2,"data")
addWorksheet(wb2,"metadata")
addWorksheet(wb2,"sector_classification")
addWorksheet(wb2,"region_classification")
addWorksheet(wb2,"100_yr_gwps")
addWorksheet(wb2,"CH4_gwps")

writeData(wb2, sheet = "info", info, colNames = F)
writeData(wb2, sheet = "metadata", meta_raw, colNames = T)
writeData(wb2, sheet = "data", edgar_raw, colNames = T)
writeData(wb2, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb2, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb2, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb2, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb2,"Results/Data/ipcc_ar6_edgar_data_essd_no_gwp.xlsx",overwrite = T)



############################################################   by gases for Robbie


gases <- edgar_raw %>% 
  group_by(year,gas) %>%
  summarise(value=sum(value))
  
gases <- spread(gases,gas,value)
write.xlsx(gases,'Results/Data/gases_essd.xlsx')

writeData(wb2, sheet = "info", info, colNames = F)
writeData(wb2, sheet = "metadata", meta_raw, colNames = T)





############################################################   for iiasa database


gas_names = c("CO2","CH4","N2O","Fgas","GHG")

iiasa_data <- edgar_ghg %>% 
  group_by(chapter_title,year) %>% 
  summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
  mutate(country="World") %>% 
  select(country,everything())

iiasa_data_regions<- edgar_ghg %>% 
  group_by(region_ar6_6,chapter_title,year) %>% 
  summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
  select(country=region_ar6_6,everything())

iiasa_data <- rbind(iiasa_data,iiasa_data_regions)


wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_iiasa")
addWorksheet(wb,"info")
addWorksheet(wb,"emissions_data")
addWorksheet(wb,"metadata")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")
addWorksheet(wb,"100_yr_gwps")
addWorksheet(wb,"CH4_gwps")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "emissions_data", iiasa_data, colNames = T)
writeData(wb, sheet = "metadata", meta, colNames = T)
writeData(wb, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_data_iiasa.xlsx",overwrite = T)




############################################################   aggregated file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_regions")

region_6_sector_totals <- edgar_ghg %>% 
  group_by(region_ar6_6,region_ar6_6_short,year,chapter_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_6_subsector_totals <- edgar_ghg %>% 
  group_by(region_ar6_6,region_ar6_6_short,year,chapter_title,subsector_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_6_code_totals <- edgar_ghg %>% 
  group_by(region_ar6_6,region_ar6_6_short,year,chapter_title,subsector_title,sector_code,fossil_bio,description) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_10_sector_totals <- edgar_ghg %>% 
  group_by(region_ar6_10,region_ar6_10_short,year,chapter_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_10_subsector_totals <- edgar_ghg %>% 
  group_by(region_ar6_10,region_ar6_10_short,year,chapter_title,subsector_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_10_code_totals <- edgar_ghg %>% 
  group_by(region_ar6_10,region_ar6_10_short,year,chapter_title,subsector_title,sector_code,fossil_bio,description) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_dev_sector_totals <- edgar_ghg %>% 
  group_by(region_ar6_dev,year,chapter_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_dev_subsector_totals <- edgar_ghg %>% 
  group_by(region_ar6_dev,year,chapter_title,subsector_title) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_dev_code_totals <- edgar_ghg %>% 
  group_by(region_ar6_dev,year,chapter_title,subsector_title,sector_code,fossil_bio,description) %>% 
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

#region_pop_gdp <- left_join(wdi_data_gdp_pop,ipcc_regions)

addWorksheet(wb,"info")
addWorksheet(wb,"metadata")
addWorksheet(wb,"region_6_sector_totals")
addWorksheet(wb,"region_6_subsector_totals")
addWorksheet(wb,"region_6_code_totals")

addWorksheet(wb,"region_10_sector_totals")
addWorksheet(wb,"region_10_subsector_totals")
addWorksheet(wb,"region_10_code_totals")

addWorksheet(wb,"region_dev_sector_totals")
addWorksheet(wb,"region_dev_subsector_totals")
addWorksheet(wb,"region_dev_code_totals")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "region_6_sector_totals", region_6_sector_totals, colNames = T)
writeData(wb, sheet = "region_6_subsector_totals", region_6_subsector_totals, colNames = T)
writeData(wb, sheet = "region_6_code_totals", region_6_code_totals, colNames = T)

writeData(wb, sheet = "region_10_sector_totals", region_10_sector_totals, colNames = T)
writeData(wb, sheet = "region_10_subsector_totals", region_10_subsector_totals, colNames = T)
writeData(wb, sheet = "region_10_code_totals", region_10_code_totals, colNames = T)

writeData(wb, sheet = "region_dev_sector_totals", region_dev_sector_totals, colNames = T)
writeData(wb, sheet = "region_dev_subsector_totals", region_dev_subsector_totals, colNames = T)
writeData(wb, sheet = "region_dev_code_totals", region_dev_code_totals, colNames = T)

writeData(wb, sheet = "metadata", meta, colNames = T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_data_regions.xlsx",overwrite = T)



#################### country file
 
countries_totals <- edgar_ghg %>%
  group_by(ISO,country,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year) %>%
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)


regions <- countries %>%
  group_by(region_ar6_10,year) %>%
  summarise_at(vars(CO2:POP),sum,na.rm=TRUE)
regions <- regions %>%
  mutate(CO2pc=CO2/POP)

wb <- createWorkbook(title = "ipcc_ar6_edgar_data_countries")
addWorksheet(wb,"info")
addWorksheet(wb,"data_countries")
addWorksheet(wb,"data_regions")
addWorksheet(wb,"metadata")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "data_countries", countries, colNames = T)
writeData(wb, sheet = "data_regions", regions, colNames = T)
writeData(wb, sheet = "metadata", meta, colNames = T)

saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_countries.xlsx",overwrite = T)



blarg <- countries %>% 
  filter(year==2018)
