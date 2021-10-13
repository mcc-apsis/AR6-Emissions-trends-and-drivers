
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar6_v5_data_ghg_gwp_ar6.RData')
load('Data/land.RData')
load('Data/ipcc_regions.RData')
load('Data/ipcc_sectors.RData')
load('Data/gwps.RData')
load('Data/WDI_gdp_pop.RData')

#################### data for Smail

# smail <- edgar_ghg %>% 
#   filter(chapter_title=="Energy systems") %>% 
#   group_by(year,region_ar6_10,region_ar6_10_short) %>% 
#   summarise(GHG=sum(GHG,na.rm=TRUE))
# 
# write.xlsx(smail,"energy_systems_10regions.xlsx",)



#################### prep

ipcc_sectors <- ipcc_sectors %>% 
  select(-IPCC.2006)

edgar_ghg <- gather(edgar_ghg,gas,value,CH4:GHG)
edgar_ghg$gas <- as.factor(edgar_ghg$gas)
edgar_ghg$gas <- fct_relevel(edgar_ghg$gas,"CO2","CH4","N2O","Fgas","GHG")
edgar_ghg <- spread(edgar_ghg,gas,value)

edgar_ghg <- edgar_ghg %>% 
  select(-region_ar6_6_short) %>% 
  filter(year>1969) %>%  
  arrange(ISO)

gwps_ch4 <- gwps_ch4 %>% 
  select(-value,-gwp_ar5,-gwp_ar4,-gwp_ar2,-gwp_ar5_feedbacks)

gwps <- gwps %>% 
  select(gas,gwp_ar6)

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
                          "This data file provides annual coverage to 2019 for all emissions sources, and up to 2020 for fossil fuel and industry CO2 emissions.",
                          "In August 2021 we updated to EDGAR version 6. Among other changes, this version adds a new `fossil_bio` column since EDGAR v5. This indicates whether a particular source is fossil or biogenic in origin. In particular, this matters for distinguishing the global warming potentials for methane sources. Previously this information was captured by an ´x` notation in the sector_code column. Please check your analysis to be sure this has no unintended effects.",
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

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_edgar6_gwp100.xlsx")
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

saveWorkbook(wb,"Results/Data/ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx",overwrite = T)


########################## NO GWPS

load('Data/edgar6_v5_data_raw.RData')

edgar_raw <- edgar_raw %>% 
  select(-gwp100_ar2,-gwp100_ar4,-gwp100_ar5_fb,-gwp100_ar5,-region_ar6_6_short) %>% 
  arrange(ISO)

info = data.frame("x" = c("Data description","Appropriate use","Global warming potentials","Year coverage","Fossil vs. bio sources","Sector codes","Region codes","Author & contact","R code","Land use data","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "This data is only authorised for use within the IPCC AR6 process, as it contains a level of detail that is not currently publically available. To use this data for non-IPCC purposes, including the publication of articles, please contact Monica Crippa at the Joint Research Centre (Monica.CRIPPA@ec.europa.eu).",
                          "This data file provides a non-aggregated list of greenhouse gases in their original units. For convenience, we also include the 100 year global warming potentials provided by working group I as a column in the dataset. Due to the size constraints, we split the raw data into 4 files with CO2, CH4 and N2O, and Fgases separately. Please see the separate file 'ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx' for data already aggregated to 100 year global warming potentials.",
                          "This data file provides annual coverage to 2019 for all emissions sources, and up to 2020 for fossil fuel and industry CO2 emissions.",
                          "In August 2021 we updated to EDGAR version 6. Among other changes, this version adds a new `fossil_bio` column since EDGAR v5. This indicates whether a particular source is fossil or biogenic in origin. In particular, this matters for distinguishing the global warming potentials for methane sources. Previously this information was captured by an ´x` notation in the sector_code column. Please check your analysis to be sure this has no unintended effects.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "CO2-LULUCF emissions are also available, but for IPCC regions only. This is available in the main 'ipcc_ar6_data_edgar6_all_gases_gwp100.xlsx' file.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_raw <- data.frame("Variable" = c("gas","gwp100_ar5","value"),
                   "Description" = c("Greenhouse gas (CO2, CH4, N2O or Fgas)","Global warming potential with a 100 year time horizon, AR6 values","Total value for each sector"),
                   "Units" = c("NA","NA","t native units"),
                   "Source" = c("NA","IPCC AR5 WG1","EDGAR_v6.0"),
                   "Citation" = c("NA","IPCC AR5 WG1",
                                  "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b"))



wb2 <- openxlsx::createWorkbook()
addWorksheet(wb2,"info")
addWorksheet(wb2,"data")
addWorksheet(wb2,"metadata")
addWorksheet(wb2,"sector_classification")
addWorksheet(wb2,"region_classification")
addWorksheet(wb2,"100_yr_gwps")
addWorksheet(wb2,"CH4_gwps")

edgar_raw_co2 <- edgar_raw %>% filter(gas=="CO2")

writeData(wb2, sheet = "info", info, colNames = F)
writeData(wb2, sheet = "metadata", meta_raw, colNames = T)
writeData(wb2, sheet = "data", edgar_raw_co2, colNames = T)
writeData(wb2, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb2, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb2, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb2, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb2,"Results/Data/ipcc_ar6_data_edgar6_CO2.xlsx",overwrite = T)


wb3 <- openxlsx::createWorkbook()
addWorksheet(wb3,"info")
addWorksheet(wb3,"data")
addWorksheet(wb3,"metadata")
addWorksheet(wb3,"sector_classification")
addWorksheet(wb3,"region_classification")
addWorksheet(wb3,"100_yr_gwps")
addWorksheet(wb3,"CH4_gwps")

edgar_raw_ch4 <- edgar_raw %>% filter(gas=="CH4")

writeData(wb3, sheet = "info", info, colNames = F)
writeData(wb3, sheet = "metadata", meta_raw, colNames = T)
writeData(wb3, sheet = "data", edgar_raw_ch4, colNames = T)
writeData(wb3, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb3, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb3, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb3, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb3,"Results/Data/ipcc_ar6_data_edgar6_CH4.xlsx",overwrite = T)


wb4 <- openxlsx::createWorkbook()
addWorksheet(wb4,"info")
addWorksheet(wb4,"data")
addWorksheet(wb4,"metadata")
addWorksheet(wb4,"sector_classification")
addWorksheet(wb4,"region_classification")
addWorksheet(wb4,"100_yr_gwps")
addWorksheet(wb4,"CH4_gwps")

edgar_raw_n2o <- edgar_raw %>% filter(gas=="N2O")

writeData(wb4, sheet = "info", info, colNames = F)
writeData(wb4, sheet = "metadata", meta_raw, colNames = T)
writeData(wb4, sheet = "data", edgar_raw_n2o, colNames = T)
writeData(wb4, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb4, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb4, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb4, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb4,"Results/Data/ipcc_ar6_data_edgar6_N2O.xlsx",overwrite = T)

wb5 <- openxlsx::createWorkbook()
addWorksheet(wb5,"info")
addWorksheet(wb5,"data")
addWorksheet(wb5,"metadata")
addWorksheet(wb5,"sector_classification")
addWorksheet(wb5,"region_classification")
addWorksheet(wb5,"100_yr_gwps")
addWorksheet(wb5,"CH4_gwps")

edgar_raw_fgas <- edgar_raw %>% filter(gas!="CO2") %>% filter(gas!="CH4") %>% filter(gas!="N2O")

writeData(wb5, sheet = "info", info, colNames = F)
writeData(wb5, sheet = "metadata", meta_raw, colNames = T)
writeData(wb5, sheet = "data", edgar_raw_fgas, colNames = T)
writeData(wb5, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb5, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb5, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb5, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb5,"Results/Data/ipcc_ar6_data_edgar6_FGAS.xlsx",overwrite = T)




meta <- data.frame("Variable" = c("blue","houghton","oscar","mean"),
                   "Description" = c("Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions","Carbon dioxide emissions"),
                   "Units" = c("tCO2","tCO2","tCO2","tCO2"),
                   "Source" = c("BLUE (bookkeeping of land use emissions) model","Houghton & Nassikas bookeeping model","OSCAR, a compact Earth system model","Mean of BLUE, H&N, OSCAR"),
                   "Citation" = c("Hansis, E., Davis, S. J., & Pongratz, J. (2015). Relevance of methodological choices for accounting of land use change carbon fluxes. Global Biogeochemical Cycles, 29(8), 1230–1246. https://doi.org/10.1002/2014GB004997",
                                  "Houghton, R. A., & Nassikas, A. A. (2017). Global and regional fluxes of carbon from land use and land cover change 1850–2015. Global Biogeochemical Cycles, 31, 456–472. https://doi.org/10.1002/2016GB005546",
                                  "Gasser, T., Crepin, L., Quilcaille, Y., Houghton, R. A., Ciais, P., & Obersteiner, M. (2020). Historical CO2 emissions from land use and land cover change and their uncertainty. Biogeosciences, 17(15), 4075–4101. https://doi.org/10.5194/bg-17-4075-2020",
                                  "Hansis et al. 2015, Houghton et al. 2017, Gasser et al. 2020"))

info = data.frame("x" = c("Data description","Appropriate use","Region codes","Author & contact","R code","","Last date of compilation","","","Sources"),
                  "y" = c("This data file provides LULUCF CO2 emissions for use in IPCC AR6. There are 3 global bookeeping models that inform the WG3 assessment of LULUCF CO2 emissions. We use the mean of these three, following the Global Carbon Project convention.",
                          "This data is only authorised for use within the IPCC AR6 process, as it contains a level of detail that is not currently publically available. To use this data for non-IPCC purposes, including the publication of articles, please contact Julia Pongratz (julia.pongratz@geographie.uni-muenchen.de).",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab"))




wb6 <- openxlsx::createWorkbook()
addWorksheet(wb6,"info")
addWorksheet(wb6,"data")
addWorksheet(wb6,"metadata")
addWorksheet(wb6,"region_classification")

writeData(wb6, sheet = "info", info, colNames = F)
writeData(wb6, sheet = "metadata", meta, colNames = T)
writeData(wb6, sheet = "data", land, colNames = T)
writeData(wb6, sheet = "region_classification",ipcc_regions,colNames=T)

saveWorkbook(wb6,"Results/Data/ipcc_ar6_data_LULUCF.xlsx",overwrite = T)

############################################################   by gases for Robbie

# 
# gases <- edgar_raw %>% 
#   group_by(year,gas) %>%
#   summarise(value=sum(value))
#   
# gases <- spread(gases,gas,value)
# write.xlsx(gases,'Results/Data/gases_essd.xlsx')
# 
# writeData(wb2, sheet = "info", info, colNames = F)
# writeData(wb2, sheet = "metadata", meta_raw, colNames = T)





############################################################   for iiasa database
# 
# 
# gas_names = c("CO2","CH4","N2O","Fgas","GHG")
# 
# iiasa_data <- edgar_ghg %>% 
#   group_by(chapter_title,year) %>% 
#   summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
#   mutate(country="World") %>% 
#   select(country,everything())
# 
# iiasa_data_regions<- edgar_ghg %>% 
#   group_by(region_ar6_6,chapter_title,year) %>% 
#   summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
#   select(country=region_ar6_6,everything())
# 
# iiasa_data <- rbind(iiasa_data,iiasa_data_regions)
# 
# 
# wb <- openxlsx::createWorkbook(title = "ipcc_ar6_data_iiasa")
# addWorksheet(wb,"info")
# addWorksheet(wb,"emissions_data")
# addWorksheet(wb,"metadata")
# addWorksheet(wb,"sector_classification")
# addWorksheet(wb,"region_classification")
# addWorksheet(wb,"100_yr_gwps")
# addWorksheet(wb,"CH4_gwps")
# 
# writeData(wb, sheet = "info", info, colNames = F)
# writeData(wb, sheet = "emissions_data", iiasa_data, colNames = T)
# writeData(wb, sheet = "metadata", meta, colNames = T)
# writeData(wb, sheet = "sector_classification",ipcc_sectors,colNames=T)
# writeData(wb, sheet = "region_classification",ipcc_regions,colNames=T)
# writeData(wb, sheet = "100_yr_gwps",gwps,colNames=T)
# writeData(wb, sheet = "CH4_gwps",gwps_ch4,colNames=T)
# 
# saveWorkbook(wb,"Results/Data/ipcc_ar6_data_iiasa.xlsx",overwrite = T)


#################### country file


info = data.frame("x" = c("Data description","LULUCF CO2","Global warming potentials","Year coverage","Region codes","Author & contact","R code","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "LULUCF CO2 data is not available for countries and is excluded from this file. Please see the region file where this is available.",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on values from AR6 WGI (see '100_yr_gwps' tab). In AR6 CH4 emissions have two separate GWPs for biogenic and fugitive sources. The specific sources and their CH4 GWPs are listed in the CH4_gwps tab.",
                          "This data file provides annual coverage to 2019 for all emissions sources, and up to 2020 for fossil fuel and industry CO2 emissions.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_countries <- data.frame("Variable" = c("gas","gwp100_ar5","value","gdp_ppp","population"),
                       "Description" = c("Greenhouse gas (CO2, CH4, N2O or Fgas)","Global warming potential with a 100 year time horizon, AR6 values","Total value for each sector","Gross Domestic Product","Population"),
                       "Units" = c("NA","NA","t native units","US $ (constant 2017 international PPP)","persons"),
                       "Source" = c("NA","IPCC AR5 WG1","EDGAR_v6.0","World Bank","World Bank"),
                       "Citation" = c("NA","IPCC AR5 WG1",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                      "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                      "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/"))


countries_totals <- edgar_ghg %>%
  group_by(ISO,country,region_ar6_6,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year) %>%
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

countries_totals <- left_join(countries_totals,wdi_data_gdp_pop %>% select(-gdp_real),by=c("year","ISO"="iso3c"))

wb7 <- createWorkbook(title = "ipcc_ar6_edgar_data_countries")
addWorksheet(wb7,"info")
addWorksheet(wb7,"data_countries")
addWorksheet(wb7,"metadata")
addWorksheet(wb7,"100_yr_gwps")

writeData(wb7, sheet = "info", info, colNames = F)
writeData(wb7, sheet = "data_countries", countries_totals, colNames = T)
writeData(wb7, sheet = "metadata", meta_countries, colNames = T)
writeData(wb7, sheet = "100_yr_gwps",gwps,colNames=T)

saveWorkbook(wb7,"Results/Data/ipcc_ar6_data_edgar6_countries.xlsx",overwrite = T)


#################### region file


info = data.frame("x" = c("Data description","LULUCF CO2","Global warming potentials","Year coverage","Region codes","Author & contact","R code","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for regions.",
                          "This data includes LULUCF CO2 emissions, which are available for up to IPCC 10 regions.",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on values from AR6 WGI (see '100_yr_gwps' tab). In AR6 CH4 emissions have two separate GWPs for biogenic and fugitive sources. The specific sources and their CH4 GWPs are listed in the CH4_gwps tab.",
                          "This data file provides annual coverage to 2019 for all emissions sources. We expect a further update in September 2021 to finalise the dataset, in particular the provisional 2019 values.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_countries <- data.frame("Variable" = c("gas","gwp100_ar5","value","gdp_ppp","population"),
                             "Description" = c("Greenhouse gas (CO2, CH4, N2O, Fgas, LULUCF CO2)","Global warming potential with a 100 year time horizon, AR6 values","Total value for each sector","Gross Domestic Product","Population"),
                             "Units" = c("NA","NA","t native units","US $ (constant 2017 international PPP)","persons"),
                             "Source" = c("NA","IPCC AR5 WG1","EDGAR_v6.0 & LULUCF Bookeeping models","World Bank","World Bank"),
                             "Citation" = c("NA","IPCC AR5 WG1",
                                            "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                            "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/",
                                            "World Bank. (2021). World Bank Development Indicators. Retrieved March 12, 2021, from http://data.worldbank.org/"))


region_totals <- edgar_ghg %>%
  group_by(region_ar6_6,region_ar6_10,region_ar6_10_short,year) %>%
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

region_totals <- left_join(region_totals,land %>% select(region_ar6_10,year,CO2_LULUCF=mean),by = c("region_ar6_10", "year"))

region_totals <- region_totals %>% 
  mutate(GHG=ifelse(region_ar6_10_short!="SEA" & region_ar6_10_short!="AIR",GHG+CO2_LULUCF,GHG))
region_totals <- region_totals %>% 
  select(year,region_ar6_6,region_ar6_10,region_ar6_10_short,CO2,CH4,N2O,Fgas,CO2_LULUCF,GHG)

blarg <- region_totals %>% 
  group_by(year) %>% 
  summarise(GHG=sum(GHG,na.rm=TRUE)/1e9)

wb8 <- createWorkbook(title = "ipcc_ar6_edgar_data_regions")
addWorksheet(wb8,"info")
addWorksheet(wb8,"data_regions")
addWorksheet(wb8,"100_yr_gwps")

writeData(wb8, sheet = "info", info, colNames = F)
writeData(wb8, sheet = "data_regions", region_totals, colNames = T)
writeData(wb8, sheet = "100_yr_gwps",gwps,colNames=T)

saveWorkbook(wb8,"Results/Data/ipcc_ar6_data_edgar6_regions.xlsx",overwrite = T)


##################### checks

# 2019 emissions

co2 <- edgar_raw_co2 %>% group_by(year) %>% summarise(value=sum(value))
ch4 <- edgar_raw_ch4 %>% group_by(year) %>% summarise(value=sum(value))
n2o <- edgar_raw_n2o %>% group_by(year) %>% summarise(value=sum(value))