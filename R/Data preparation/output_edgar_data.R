
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar_data_gwp_ar5.RData')

#################### prep

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  filter(year>1969) %>% 
  select(-category_1,-category_2,-category_3,-region_ar6_5_short) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

load('Data/basic.RData')
load('Data/tsu_codes.RData')
load('Data/ipcc_categories.RData')
load('Data/gwps.RData')

basic <- basic %>% 
  select(ISO,year=Year,GDP=gdp_ppp_WB,POP=pop_UN) %>% 
  filter(year>1969)

basic <- left_join(tsu_codes,basic,by=("ISO"="ISO")) %>% 
  filter(!is.na(year))

edgar_categories <- edgar_GHG_ar5 %>% 
  select(sector_code,chapter,chapter_title,description) %>% 
  unique() %>% 
  arrange(chapter)


#################### meta data

meta <- data.frame("Variable" = c("CO2","CH4","N2O","Fgas","GHG","GDP","POP"),
                   "Description" = c("Carbon emissions","Methane emissions","Nitrous oxide emissions","Flourinated gas emissions","Total greenhouse gas emissions","Gross Domestic Product","Population"),
                   "Units" = c("tCO2","tCO2eq","tCO2eq","tCO2eq","tCO2eq","persons","US $ (constant 2011 international PPP)"),
                   "Source" = c("EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","World Bank","UN Department of Economic and Social Affairs"),
                   "Citation" = c("Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "World Bank. (2019). World Bank Development Indicators. Retrieved November 7, 2019, from http://data.worldbank.org/",
                                  "UN DESA. (2018). World Urbanization Prospects: The 2018 Revision. New York: United Nations, Department of Economic and Social Affairs, Population Division."))


#################### info

info = data.frame("x" = c("Data description","Global warming potentials","Year coverage","Sector codes","Region codes","Author & contact","R code","Land use data","","Last update","","","Emissions data source"),
                  "y" = c("This datafile provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on AR5 values (see '100_yr_gwps' tab). We will update to AR6 GWPs once these are fully provided by WGII.",
                          "This provisional dataset provides annual coverage to 2018 for all emissions sources, but more limited annual coverage for GDP data.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "We are unable to provide land use data in this iteration of the data.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))



#################### full file

wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_full")
addWorksheet(wb,"info")
addWorksheet(wb,"emissions_data")
addWorksheet(wb,"supplementary_data")
addWorksheet(wb,"metadata")
addWorksheet(wb,"sector_classification")
addWorksheet(wb,"region_classification")
addWorksheet(wb,"100_yr_gwps")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "emissions_data", edgar_GHG_ar5, colNames = T)
writeData(wb, sheet = "supplementary_data", basic, colNames = T)
writeData(wb, sheet = "metadata", meta, colNames = T)
writeData(wb, sheet = "sector_classification",edgar_categories,colNames=T)
writeData(wb, sheet = "region_classification",tsu_codes,colNames=T)
writeData(wb, sheet = "100_yr_gwps",gwps,colNames=T)


saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_gwp100.xlsx",overwrite = T)




########################## NO GWPS


load('Data/edgar_data_all.RData')



info = data.frame("x" = c("Data description","Global warming potentials","Year coverage","Sector codes","Region codes","Author & contact","R code","Land use data","","Last update","","","Emissions data source"),
                  "y" = c("This datafile provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "This datafile provides a non-aggregated list of greenhouse gases in their original units. Please see the separate file 'ipcc_ar6_edgar_data_gwp100.xlsx' for data aggregated to 100 year global warming potentials.",
                          "This provisional dataset provides annual coverage to 2018 for all emissions sources, but more limited annual coverage for GDP data.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "We are unable to provide land use data in this iteration of the data.",
                          "",
                          as.character(Sys.time()),
                          "",
                          "",
                          "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610"                  ))

meta <- data.frame("Variable" = c("CO2","CH4","N2O","Fgas","GHG","GDP","POP"),
                   "Description" = c("Carbon emissions","Methane emissions","Nitrous oxide emissions","Flourinated gas emissions","Total greenhouse gas emissions","Gross Domestic Product","Population"),
                   "Units" = c("t","t","t","t","t","persons","US $ (constant 2011 international PPP)"),
                   "Source" = c("EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","World Bank","UN Department of Economic and Social Affairs"),
                   "Citation" = c("Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "World Bank. (2019). World Bank Development Indicators. Retrieved November 7, 2019, from http://data.worldbank.org/",
                                  "UN DESA. (2018). World Urbanization Prospects: The 2018 Revision. New York: United Nations, Department of Economic and Social Affairs, Population Division."))



wb2 <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_nogwps")
addWorksheet(wb2,"info")
addWorksheet(wb2,"emissions_data")
addWorksheet(wb2,"supplementary_data")
addWorksheet(wb2,"metadata")
addWorksheet(wb2,"sector_classification")
addWorksheet(wb2,"region_classification")
addWorksheet(wb2,"100_yr_gwps")

writeData(wb2, sheet = "info", info, colNames = F)
writeData(wb2, sheet = "emissions_data", edgar_GHG, colNames = T)
writeData(wb2, sheet = "supplementary_data", basic, colNames = T)
writeData(wb2, sheet = "metadata", meta, colNames = T)
writeData(wb2, sheet = "sector_classification",edgar_categories,colNames=T)
writeData(wb2, sheet = "region_classification",tsu_codes,colNames=T)
writeData(wb2, sheet = "100_yr_gwps",gwps,colNames=T)


saveWorkbook(wb2,"Results/Data/ipcc_ar6_edgar_data_no_gwp.xlsx",overwrite = T)


############################################################   regional file

# regions <- edgar_GHG %>%
#   group_by(region_ar6_5,year,chapter,sector_code,description) %>%
#   summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)
#
# wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_regions")
# addWorksheet(wb,"info")
# addWorksheet(wb,"data")
# addWorksheet(wb,"metadata")
#
# writeData(wb, sheet = "info", info, colNames = F)
# writeData(wb, sheet = "data", regions, colNames = T)
# writeData(wb, sheet = "metadata", meta %>% filter(Variable!="GDP") %>% filter(Variable!="POP"), colNames = T)
#
# saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_regions.xlsx",overwrite = T)
# 
# 
# #################### chapter file
# 
# chapters <- edgar_GHG %>% 
#   group_by(chapter,year,sector_code,description) %>%
#   summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)
# 
# 
# wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_chapters")
# addWorksheet(wb,"info")
# addWorksheet(wb,"data")
# addWorksheet(wb,"metadata")
# 
# writeData(wb, sheet = "info", info, colNames = F)
# writeData(wb, sheet = "data", chapters, colNames = T)
# writeData(wb, sheet = "metadata", meta %>% filter(Variable!="GDP") %>% filter(Variable!="POP"), colNames = T)
# 
# saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_chapters.xlsx",overwrite = T)
# 
# #################### country file
# 
# countries <- edgar_GHG %>% 
#   group_by(ISO,country,region_ar6_5,year) %>%
#   summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)
# 
# countries <- left_join(countries,basic,by=c("ISO"="ISO","year"="Year"))
# 
# wb <- createWorkbook(title = "ipcc_ar6_edgar_data_countries")
# addWorksheet(wb,"info")
# addWorksheet(wb,"data")
# addWorksheet(wb,"metadata")
# 
# writeData(wb, sheet = "info", info, colNames = F)
# writeData(wb, sheet = "data", countries, colNames = T)
# writeData(wb, sheet = "metadata", meta, colNames = T)
# 
# saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_countries.xlsx",overwrite = T)
