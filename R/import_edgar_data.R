
rm(list = ls())
library(xlsx)
library(tidyverse)

### EDGAR data

########### load sheets ########### 

edgar_CO2 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='CO2_FT2018',startRow=5)
names(edgar_CO2) <- gsub(x = names(edgar_CO2), pattern = "Y_", replacement = "")  

edgar_CO2 <- gather(edgar_CO2,Year,Value,'1970':'2018') %>% 
  select(ISO_A3=Country_code_A3,EDGAR_country=Name,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CO2 = Value)



edgar_CH4 <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='CH4',startRow=5)
names(edgar_CH4) <- gsub(x = names(edgar_CH4), pattern = "Y_", replacement = "")  

edgar_CH4 <- gather(edgar_CH4,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,EDGAR_country=Name,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,CH4 = Value)



edgar_N2O <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                 sheet='N2O',startRow=5)
names(edgar_N2O) <- gsub(x = names(edgar_N2O), pattern = "Y_", replacement = "")  

edgar_N2O <- gather(edgar_N2O,Year,Value,'1970':'2015') %>% 
  select(ISO_A3=Country_code_A3,EDGAR_country=Name,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,N2O = Value)



edgar_Fgas <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/INPUT_IPCC_WG3_EDGAR_v5.0_FT2018_CO2_GHG_AR4_AR5.xlsx',
                                  sheet='GWP_100_AR5_Fgases',startRow=5)
names(edgar_Fgas) <- gsub(x = names(edgar_Fgas), pattern = "Y_", replacement = "")  

edgar_Fgas <- gather(edgar_Fgas,Year,Value,'1970':'2015')%>% 
  select(ISO_A3=Country_code_A3,EDGAR_country=Name,Year,IPCC.detailed=IPCC_for_std_report_detailed,IPCC_detailed_description=IPCC_for_std_report_detailed_desc,Fgas = Value)


########### join sheets ########### 

edgar_GHG <- full_join(edgar_CO2,edgar_CH4)
edgar_GHG <- full_join(edgar_GHG,edgar_N2O)
edgar_GHG <- full_join(edgar_GHG,edgar_Fgas)

########### GWPS and units ########### 

## GWP values (https://www.ghgprotocol.org/sites/default/files/ghgp/Global-Warming-Potential-Values%20%28Feb%2016%202016%29_1.pdf)

# CO2: 1
# CH4: 28
# N2O: 265
# Fgas: diverse (already converted by JRC)

edgar_GHG <- edgar_GHG %>% 
  mutate(CH4 = CH4*28) %>% 
  mutate(N2O = N2O*265)

# to convert back to original
# 1/GWP


### convert to tCO2
edgar_GHG <- edgar_GHG %>% 
  mutate(CO2 = CO2*1000) %>% 
  mutate(CH4 = CH4*1000) %>% 
  mutate(N2O = N2O*1000) %>% 
  mutate(Fgas = Fgas*1000)


########### calculate total GHG ########### 

edgar_GHG <- edgar_GHG %>% 
  group_by(ISO_A3,EDGAR_country,Year,IPCC.detailed) %>% 
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(Year=as.numeric(Year))

edgar_GHG <- edgar_GHG %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

#rm(edgar_CH4,edgar_CO2,edgar_Fgas,edgar_N2O)

########### join source categories from compilation (see category_mapping.R) ########### 

load('Data/ipcc_categories.RData')

edgar_GHG <- left_join(edgar_GHG,ipcc_categories %>% select(code,description,category_1,category_2,category_3,IPCC_AR6_chapter),by=c("IPCC.detailed"="code"))

edgar_GHG <- edgar_GHG %>% 
  select(ISO=ISO_A3,sector_code=IPCC.detailed,chapter=IPCC_AR6_chapter,description,category_1,category_2,category_3,Year,everything(),-IPCC_detailed_description)

########### join country names and World Bank income classification ########### 

codes <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'ISO_master')
edgar_GHG <- left_join(edgar_GHG,codes %>% select(name,alpha.3,WB.income),by=c("ISO"="alpha.3"))
edgar_GHG$WB.income <- as.factor(edgar_GHG$WB.income)
edgar_GHG$WB.income <- factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])

## identify additional countries in EDGAR

missing <- edgar_GHG %>% 
  filter(is.na(name)) %>% 
  select(ISO,EDGAR_country) %>% 
  unique()

edgar_GHG <- edgar_GHG %>% 
  mutate(name=ifelse(is.na(name),EDGAR_country,name))

rm(codes)
########### allocate World Bank NAs to low income ########### 

edgar_GHG <- edgar_GHG %>% 
  mutate(WB.income = ifelse(is.na(WB.income),"Low income",as.character(WB.income))) %>% 
  mutate(WB.income = as.factor(WB.income))
edgar_GHG$WB.income = factor(edgar_GHG$WB.income,levels(edgar_GHG$WB.income)[c(1,4,3,2)])



########### join country categorisation from WGIII TSU ###########

load('Data/tsu_codes.R')

edgar_GHG <- left_join(edgar_GHG,tsu_codes %>% select(-name),by=c("ISO"="ISO"))

missing <- edgar_GHG %>% 
  filter(is.na(region_ar6_5) | is.na(region_ar6_10) | is.na(region_ar6_22) | is.na(region_ar6_dev)) %>% 
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

edgar_GHG$region_ar6_5 <- factor(edgar_GHG$region_ar6_5)
edgar_GHG$region_ar6_5 <- factor(edgar_GHG$region_ar6_5,levels(edgar_GHG$region_ar6_5)[c(5,6,1,2,3,4,7)])


############## tidying up 

edgar_GHG <- edgar_GHG %>% 
  select(ISO,country=name,region_ar6_5,region_ar6_10,region_ar6_22,region_ar6_dev,year=Year,chapter,sector_code,description,category_1,category_2,category_3,CO2:GHG)

############## ADD OLD DATA

load('Data/edgar_old.RData')

# where does data end?
blarg <- edgar_GHG %>%  
   group_by(ISO,year) %>% 
   summarise(CO2=sum(CO2,na.rm=T),CH4=sum(CH4,na.rm=T),N2O=sum(N2O,na.rm=T),Fgas=sum(Fgas,na.rm=T))

edgar_GHG_old <- edgar_GHG_old %>% 
  filter(year>2015) %>% 
  select(-CO2,-GHG)

z <- edgar_GHG_old %>% filter(is.na(ISO))

edgar_GHG_old <- edgar_GHG_old %>% 
  filter(!is.na(ISO))

new <- edgar_GHG %>% 
  filter(year>2015) %>% 
  select(-CH4,-N2O,-Fgas,-GHG)

merged <- full_join(edgar_GHG_old,new)
merged <- merged %>% 
  group_by(ISO,year,chapter,sector_code) %>%
  mutate(GHG = sum(CO2,CH4,N2O,Fgas,na.rm=T)) %>% 
  ungroup()

merged <- merged %>% 
  mutate(region_ar6_5=ifelse(region_ar6_5=="Eastern Europe and West-Central Asia\n\n","Eastern Europe and West-Central Asia",region_ar6_5)) %>% 
  mutate(region_ar6_5=ifelse(region_ar6_5=="Developed Countries\n\n","Developed Countries",region_ar6_5))

edgar_GHG <- edgar_GHG %>% 
  filter(year<=2015)
edgar_GHG <- rbind(edgar_GHG,merged)

sum(edgar_GHG$CO2,na.rm=T)
sum(blarg$CH4,na.rm=T)
sum(blarg$N2O,na.rm=T)
sum(blarg$Fgas,na.rm=T)


############# set NAs in GHG 2018

edgar_GHG$GHG[edgar_GHG$year==2018] <- NA

edgar_GHG <- edgar_GHG %>% 
  mutate(GHG = ifelse(is.na(CO2) & is.na(CH4) & is.na(N2O) & is.na(Fgas),NA,GHG))

save(edgar_GHG,file='Data/edgar.RData')
