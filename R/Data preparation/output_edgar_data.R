
rm(list = ls())
library(tidyverse)
library(openxlsx)
load('Data/edgar_data_gwp_ar6.RData')

#################### prep

edgar_GHG_ar6 <- edgar_GHG_ar6 %>% 
  filter(year>1969) %>% 
  filter(sector_code!="Total") %>% 
  arrange(ISO)

#load('Data/basic.RData')
load('Data/ipcc_regions.RData')
load('Data/ipcc_sectors.RData')
load('Data/gwps.RData')

gwps <- gwps %>% 
  mutate(gwp_ar6=ifelse(gas=="CH4",NA,gwp_ar6)) %>% 
  mutate(gas=ifelse(gas=="CH4","CH4 (see CH4 tab)",gas)) 

gwps_ch4 <- gwps_ch4 %>% 
  select(sector_code,gas,gwp_ar6=value)

# basic <- basic %>% 
#   select(ISO,year=Year,GDP=gdp_ppp_WB,POP=pop_UN) %>% 
#   filter(year>1969)
# 
# basic <- left_join(ipcc_regions,basic,by=("ISO"="ISO")) %>% 
#   filter(!is.na(year))


#################### meta data

meta <- data.frame("Variable" = c("CO2","CH4","N2O","Fgas","GHG","GDP","POP"),
                   "Description" = c("Carbon emissions","Methane emissions","Nitrous oxide emissions","Flourinated gas emissions","Total greenhouse gas emissions","Gross Domestic Product","Population"),
                   "Units" = c("tCO2","tCO2eq","tCO2eq","tCO2eq","tCO2eq","US $ (constant 2011 international PPP)","persons"),
                   "Source" = c("EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","EDGAR_v5.0","World Bank","UN Department of Economic and Social Affairs"),
                   "Citation" = c("Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "Crippa, M., Oreggioni, G., Guizzardi, D., Muntean, M., Schaaf, E., Lo Vullo, E., Solazzo, E., Monforti-Ferrario, F., Olivier, J.G.J., Vignati, E., Fossil CO2 and GHG emissions of all world countries - 2019 Report, EUR 29849 EN, Publications Office of the European Union, Luxembourg, 2019, ISBN 978-92-76-11100-9, doi:10.2760/687800, JRC117610",
                                  "World Bank. (2019). World Bank Development Indicators. Retrieved November 7, 2019, from http://data.worldbank.org/",
                                  "UN DESA. (2018). World Urbanization Prospects: The 2018 Revision. New York: United Nations, Department of Economic and Social Affairs, Population Division."))


#################### info

info = data.frame("x" = c("Data description","Appropriate use","Global warming potentials","Year coverage","Sector codes","Region codes","Author & contact","R code","Land use data","","Last update","","","Emissions data source"),
                  "y" = c("This datafile provides detailed emissions accounts for use in IPCC AR6, as well as supplementary GDP and population data for countries.",
                          "This data is only authorised for use within the IPCC process, as it contains a level of detail that is not currently publically available. To use this data for non-IPCC purposes, please contact Monica Crippa at the Joint Research Centre (Monica.CRIPPA@ec.europa.eu).",
                          "CH4, N2O and Fgas emissions are converted to 100 year global warming potentials based on values from AR6 WGI (see '100_yr_gwps' tab). These may be updated again by WGI before final publication. In this case we will update and replace this file. In AR6 CH4 emissions have two separate GWPs for biogenic and fugitive sources. These are listed in the CH4_gwps tab.",
                          "This provisional dataset provides annual coverage to 2018 for all emissions sources, but more limited annual coverage for GDP data.",
                          "Emission sector codes have been allocated to major sectors/AR6 chapters on consultation with the chapter leads. A full list and description of codes is in the sector_classification tab.",
                          "Countries have been allocated to regions by the WGIII Technical Support Unit. A summary of the country and region codes is in the region_classification tab.",
                          "William F. Lamb (Lamb@mcc-berlin.net) compiled this data from underlying sources (see metadata).",
                          "The code for compiling this data (as well as summary figures for AR6 Ch2) is available online: https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers",
                          "We are unable to provide land use CO2 data in this iteration of the data.",
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
addWorksheet(wb,"CH4_gwps")

writeData(wb, sheet = "info", info, colNames = F)
writeData(wb, sheet = "emissions_data", edgar_GHG_ar6, colNames = T)
writeData(wb, sheet = "supplementary_data", basic, colNames = T)
writeData(wb, sheet = "metadata", meta, colNames = T)
writeData(wb, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb, sheet = "CH4_gwps",gwps_ch4,colNames=T)

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
                   "Units" = c("t","t","t","t","t","US $ (constant 2011 international PPP)","persons"),
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
addWorksheet(wb2,"CH4_gwps")

writeData(wb2, sheet = "info", info, colNames = F)
writeData(wb2, sheet = "emissions_data", edgar_GHG, colNames = T)
writeData(wb2, sheet = "supplementary_data", basic, colNames = T)
writeData(wb2, sheet = "metadata", meta, colNames = T)
writeData(wb2, sheet = "sector_classification",ipcc_sectors,colNames=T)
writeData(wb2, sheet = "region_classification",ipcc_regions,colNames=T)
writeData(wb2, sheet = "100_yr_gwps",gwps,colNames=T)
writeData(wb2, sheet = "CH4_gwps",gwps_ch4,colNames=T)

saveWorkbook(wb2,"Results/Data/ipcc_ar6_edgar_data_no_gwp.xlsx",overwrite = T)


############################################################   sectors by gas

gas_names = c("CO2","CH4","N2O","Fgas","GHG")

gases <- edgar_GHG_ar6 %>% 
  filter(year==2018) %>% 
  group_by(sector_code,description,subsector,subsector_title,chapter,chapter_title) %>% 
  summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
  mutate_at(vars(gas_names),list(~./1e9))

wb3 <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_gases")

for (i in 1:length(gas_names)) {
  
  addWorksheet(wb3,gas_names[i])
  data <- gases %>% 
    filter(get(gas_names[i])>0) %>% 
    select(sector_code,description,subsector,subsector_title,chapter,chapter_title,value_2018=gas_names[i])
  writeData(wb3, sheet = gas_names[i], data, colNames = T)
  
}
  
saveWorkbook(wb3,"Results/Data/ipcc_ar6_edgar_gases.xlsx",overwrite = T)



############################################################   for iiasa database

gas_names = c("CO2","CH4","N2O","Fgas","GHG")

iiasa_data <- edgar_GHG_ar6 %>% 
  group_by(chapter_title,year) %>% 
  summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
  mutate(country="World") %>% 
  select(country,everything())

iiasa_data_regions<- edgar_GHG_ar6 %>% 
  group_by(region_ar6_5,chapter_title,year) %>% 
  summarise_at(vars(gas_names),sum,na.rm=TRUE) %>% 
  select(country=region_ar6_5,everything())

iiasa_data <- rbind(iiasa_data,iiasa_data_regions)


wb <- openxlsx::createWorkbook(title = "ipcc_ar6_edgar_data_iiasa")
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

saveWorkbook(wb,"Results/Data/ipcc_ar6_edgar_data_iiasa.xlsx",overwrite = T)




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
countries <- edgar_GHG_ar6 %>%
  group_by(ISO,country,region_ar6_5,region_ar6_10,region_ar6_22,region_ar6_dev,year) %>%
  summarise_at(vars(CO2:GHG),sum,na.rm=TRUE)

basic <- basic %>% select(ISO,year,GDP,POP)

countries <- left_join(countries,basic,by=c("ISO"="ISO","year"="year"))

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
