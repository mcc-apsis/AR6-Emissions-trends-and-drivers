
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/Not public/IPCC data versions/edgar6_v5_data_raw.RData')
load('Data/data_land_co2.RData')
load('Data/ipcc_regions.RData')
load('Data/ipcc_sectors.RData')
load('Data/gwps.RData')
load('Data/data_WDI_gdp_pop.RData')

gwps_ch4 <- gwps_ch4 %>% select(-gwp100_ar4,-gwp100_ar2,-gwp100_ar5_feedbacks)
gwps <- gwps %>% select(gas,gwp100_ar5,gwp100_ar6)
gwps <- gwps %>% filter(gas!="CH4")

ipcc_codes <- read.xlsx('Data/Codes and classifications/EDGAR_IPCC1996_to_IPCC2006.xlsx',sheet=3)

ipcc_sectors <- left_join(ipcc_sectors,ipcc_codes %>% select(-description,-ipcc_1996),by=c("code"="sector_code"))
ipcc_sectors <- ipcc_sectors %>%
  select(EDGAR_code=code,fossil_bio,sector_title=IPCC_AR6_chapter_title,subsector_title,description,IPCC_2006=ipcc_2006) %>% 
  group_by(EDGAR_code,fossil_bio,sector_title,description,subsector_title) %>% 
  summarise(IPCC_2006=paste0(IPCC_2006,collapse=", ")) %>% 
  filter(EDGAR_code!="IEA DATA")


#################### raw data no gwps

edgar_raw <- edgar_raw %>% 
  select(-chapter,-subsector)

edgar_raw <- edgar_raw %>% 
  group_by(ISO,country,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year,sector_title=chapter_title,subsector_title,gas,gwp100_ar2,gwp100_ar4,gwp100_ar5_feedbacks,gwp100_ar5,gwp100_ar6) %>% 
  summarise(value=sum(value,na.rm=TRUE))

#################### quick check
summary <- edgar_raw %>% 
  group_by(year,gas) %>% 
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  filter(year==2019)


info_raw = data.frame("x" = c("Data description","Global warming potentials","Sectors","Regions","Author & contact","R code","Land use data","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                  "y" = c("This data file provides global, regional, national, and sectoral estimates of greenhouse gas (GHG) emissions from 1970-2019",
                          "Emissions by gas are provided in tons of original units. For convenience, we also include 100 year global warming potentials from the IPCC 5th and 6th Assessment Reports as columns in the dataset. Please see the separate file 'essd_ghg_data_gwp100.xlsx' for data already aggregated to 100 year (AR6) global warming potentials.",
                          "Emissions have been allocated to major sectors corresponding to the IPCC AR6 chapters on consultation with the chapter leads. A further breakdown by subsector is also available. A full list and description of how emissions codes match to sectors and subsectors is in the sector_classification tab.",
                          "Countries have been allocated to regions by the IPCC AR6 WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "CO2-LULUCF emissions are available as a separate file ('essd_lulucf_data.xlsx') for IPCC regions only.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "See metadata tab",
                          "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_raw <- data.frame("Variable" = c("gas","gwp100_ar5","gwp100_ar6","value"),
                       "Description" = c("Greenhouse gas (CO2, CH4, N2O or Fgas)","Global warming potential with a 100 year time horizon, AR5 values","Global warming potential with a 100 year time horizon, AR6 values","Total value for each sector"),
                       "Units" = c("NA","NA","NA","t native units"),
                       "Source" = c("NA","IPCC AR5 WG1","IPCC AR6 WG1 Ch7","EDGAR_v6.0"),
                       "Citation" = c("NA","IPCC AR5 WG1","IPCC AR6 WG1 Ch7",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b"))



wb1 <- openxlsx::createWorkbook(title = "essd_ghg_data.xlsx")
addWorksheet(wb1,"info")
addWorksheet(wb1,"metadata")
addWorksheet(wb1,"data")
addWorksheet(wb1,"sector_classification")
addWorksheet(wb1,"region_classification")
addWorksheet(wb1,"100_yr_gwps")
addWorksheet(wb1,"CH4_gwps")

writeData(wb1, sheet = "info", info_raw, colNames = F)
writeData(wb1, sheet = "metadata", meta_raw, colNames = T)
writeData(wb1, sheet = "data", edgar_raw %>% 
            select(-gwp100_ar2,-gwp100_ar4,-gwp100_ar5_feedbacks), colNames = T)
writeData(wb1, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb1, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb1, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb1, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb1,"Results/Data/essd_ghg_data.xlsx",overwrite = T)


#################### aggregated data
load('Data/Not public/IPCC data versions/edgar6_v5_data_ghg_gwp100_ar6.RData')

edgar_ghg <- edgar_ghg %>% 
  select(-chapter,-subsector)

edgar_ghg <- edgar_ghg %>% 
  group_by(ISO,country,region_ar6_6,region_ar6_6_short,region_ar6_10,region_ar6_10_short,region_ar6_22,region_ar6_dev,year,sector_title=chapter_title,subsector_title) %>% 
  summarise_at(vars(CO2,CH4,N2O,Fgas,GHG),sum,na.rm=TRUE)

#################### quick check
summary <- edgar_ghg %>% 
  group_by(year) %>% 
  summarise_at(vars(CO2,CH4,N2O,Fgas,GHG),sum,na.rm=TRUE) %>% 
  filter(year==2019)


info_ghg = data.frame("x" = c("Data description","Global warming potentials","Sectors","Regions","Author & contact","R code","Land use data","","Last date of compilation","","","Sources","Link to public EDGAR version"),
                      "y" = c("This data file provides global, regional, national, and sectoral estimates of greenhouse gas (GHG) emissions from 1970-2019",
                              "Emissions by gas are provided in tons of CO2 equivalent, based on 100 year global warming potentials from the IPCC 6th Assessment Report. We also aggregate all Fgases into a single variable. Please see the separate file 'essd_ghg_data.xlsx' for data in original gas units.",
                              "Emissions have been allocated to major sectors corresponding to the IPCC AR6 chapters on consultation with the chapter leads. A further breakdown by subsector is also available. A full list and description of how emissions codes match to sectors and subsectors is in the sector_classification tab.",
                              "Countries have been allocated to regions by the IPCC AR6 WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                              "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                              "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                              "CO2-LULUCF emissions are available as a separate file ('essd_lulucf_data.xlsx') for IPCC regions only.",
                              "",
                              as.character(Sys.time()),
                              "",
                              "",
                              "See metadata tab",
                              "https://edgar.jrc.ec.europa.eu/dataset_ghg60"))



meta_ghg <- data.frame("Variable" = c("CO2","CH4","N2O","Fgas","GHG"),
                       "Description" = c("Carbon dioxide emissions","Methane emissions","Nitrous Oxide emissions","Fluorinated gas emissions","Total greenhouse gas emissions"),
                       "Units" = c("tCO2eq","tCO2eq","tCO2eq","tCO2eq","tCO2eq"),
                       "Source" = c("EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0","EDGAR_v6.0"),
                       "Citation" = c("Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b",
                                      "Crippa, Monica; Guizzardi, Diego; Muntean, Marilena; Schaaf, Edwin; Lo Vullo, Eleonora; Solazzo, Efisio; Monforti-Ferrario, Fabio; Olivier, Jos; Vignati, Elisabetta (2021):  EDGAR v6.0 Greenhouse Gas Emissions. European Commission, Joint Research Centre (JRC) [Dataset] PID: http://data.europa.eu/89h/97a67d67-c62e-4826-b873-9d972c4f670b"))


wb2 <- openxlsx::createWorkbook(title = "essd_ghg_data_gwp100.xlsx")
addWorksheet(wb2,"info")
addWorksheet(wb2,"metadata")
addWorksheet(wb2,"data")
addWorksheet(wb2,"sector_classification")
addWorksheet(wb2,"region_classification")
addWorksheet(wb2,"100_yr_gwps")
addWorksheet(wb2,"CH4_gwps")

writeData(wb2, sheet = "info", info_ghg, colNames = F)
writeData(wb2, sheet = "metadata", meta_ghg, colNames = T)
writeData(wb2, sheet = "data", edgar_ghg, colNames = T)
writeData(wb2, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb2, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb2, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb2, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb2,"Results/Data/essd_ghg_data_gwp100.xlsx",overwrite = T)




########################### land



info_land = data.frame(x=c("Info","Units","Author","Contact","","Sources","blue","houghton","oscar","mean"),
                  y=c("This data file provides historical estimates of CO2 emissions from land use, land use change and forestry. It is based on three land use bookeeping models (BLUE, Houghton & Nassikas, OSCAR). Following a practice established by the global carbon budget, and recently the Working Group I of the IPCC (6th Assessment Report), CO2-LULUCF emissions are taken as the mean of these three models.",
                      "tCO2",
                      "Julia Pongratz",
                      "William F. Lamb (Lamb@mcc-berlin.net)",
                      "",
                      "",
                      "Hansis, E., Davis, S. J., & Pongratz, J. (2015). Relevance of methodological choices for accounting of land use change carbon fluxes. Global Biogeochemical Cycles, 29(8), 1230–1246. https://doi.org/10.1002/2014GB004997",
                      "Houghton, R. A., & Nassikas, A. A. (2017). Global and regional fluxes of carbon from land use and land cover change 1850–2015. Global Biogeochemical Cycles, 31, 456–472. https://doi.org/10.1002/2016GB005546",
                      "Gasser, T., Crepin, L., Quilcaille, Y., Houghton, R. A., Ciais, P., & Obersteiner, M. (2020). Historical CO2 emissions from land use and land cover change and their uncertainty. Biogeosciences, 17(15), 4075–4101. https://doi.org/10.5194/bg-17-4075-2020",
                      "Hansis et al. 2015, Houghton & Nassikas 2017, Gasser et al. 2020"))

wb3 <- createWorkbook()
addWorksheet(wb3,"info")
addWorksheet(wb3,"data")
addWorksheet(wb3,"region_classification")

writeData(wb3, sheet = "info", info_land, colNames = F, rowNames = F)
writeData(wb3, sheet = "data", land, colNames = T, rowNames = F)
writeData(wb3, sheet = "region_classification",ipcc_regions,colNames=T)

saveWorkbook(wb3,"Results/Data/essd_lulucf_data.xlsx",overwrite=T)


save(edgar_ghg,edgar_raw,file="Data/data_edgar_ghg.RData")
