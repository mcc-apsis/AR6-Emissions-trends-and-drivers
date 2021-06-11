
load('Data/edgar6_data_ghg_gwp_ar5.RData')

data <- edgar_ghg %>% 
  group_by(description,sector_code,fossil_bio,chapter_title) %>% 
  summarise_at(vars(CO2,N2O,CH4,Fgas),sum,na.rm=T)

data <- data %>% 
  mutate(gases=ifelse(abs(CO2)>0,"CO2",NA))

data <- data %>% 
  mutate(gases=ifelse(abs(N2O)>0,paste0(gases,", N2O"),gases)) %>% 
  mutate(gases=ifelse(abs(CH4)>0,paste0(gases,", CH4"),gases)) %>% 
  mutate(gases=ifelse(abs(Fgas)>0,paste0(gases,", Fgas"),gases))
  
data$gases <- gsub("NA, ","",data$gases)

data <- data %>% 
  arrange(chapter_title,sector_code)

openxlsx::write.xlsx(data,"edgar_gases.xlsx")



vars <- names(edgar_GHG)
vars <- vars [15:37]

load('Data/edgar_data_all.RData')
data <- edgar_GHG %>% 
  group_by(year) %>% 
  summarise_at(all_of(vars),sum,na.rm=T)

openxlsx::write.xlsx(data,"data.xlsx")
