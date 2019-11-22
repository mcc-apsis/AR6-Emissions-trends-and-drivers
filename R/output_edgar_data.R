
rm(list = ls())
library(tidyverse)
library(ggpubr)
library(gganimate)
library(openxlsx)
load('Data/edgar.RData')

#################### prep

edgar_GHG <- edgar_GHG %>% 
  filter(year>1969) %>% 
  select(-category_1,-category_2,-category_3) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

load('Data/basic.RData')
load('Data/tsu_codes.R')
load('Data/ipcc_categories.RData')

basic <- basic %>% 
  select(ISO,year=Year,GDP=gdp_ppp_WB,POP=pop_UN) %>% 
  filter(year>1969)

basic <- left_join(tsu_codes,basic,by=("ISO"="ISO")) %>% 
  filter(!is.na(year))


ipcc_categories <- ipcc_categories %>% 
  select(code,ipcc_ar6_chapter=IPCC_AR6_chapter,description)

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

info = data.frame("x" = c("Data description","Global warming potentials","Year coverage","Sector codes","Region codes","Author & contact","R code","Land use data","","","Emissions data source"),
                  "y" = c("This datafile provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on AR5 values (see: https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf).",
                          "This provisional dataset provides annual coverage to 2017 for all emissions sources, and to 2018 for CO2 only.",
                          "Emissions sector codes have been allocated to AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "We are unable to provide land use data in this first iteration.",
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

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "emissions_data", edgar_GHG, colNames = T)
writeData(wb, sheet = "supplementary_data", basic, colNames = T)
writeData(wb, sheet = "metadata", meta, colNames = T)
writeData(wb, sheet = "sector_classification",ipcc_categories,colNames=T)
writeData(wb, sheet = "region_classification",tsu_codes,colNames=T)


saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_full.xlsx",overwrite = T)









#################### regional file

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
