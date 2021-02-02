
load('Data/edgar_data_all.RData')

data <- edgar_GHG %>% 
  group_by(year,sector_code) %>% 
  summarise(CH4=sum(CH4,na.rm=TRUE))

data <- spread(data,year,CH4)

openxlsx::write.xlsx(data,"CH4_native.xlsx")
