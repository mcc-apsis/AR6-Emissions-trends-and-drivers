rm(list = ls())
library(tidyverse)
library(openxlsx)
library(ggplot2)

crf_data<-read_csv("../../Data/supplemetary data/Guetschow-et-al-2020-PRIMAP-crf_2020-v1.csv")
#head(crf_data)

load("../../Data/edgar_data_all.RData")
load('../../Data/gwps.RData')


# head(edgar_GHG)
# unique(edgar_GHG$sector_code)
# gsub("IPC","",unique(crf_data$category))


#comp_sector<-left_join(crf_data,as_tibble(unique(edgar_GHG$sector_code)),by=(category=sector_code))

#crf_data<- crf_data %>%
#  mutate(sector_code=gsub("IPC","",category)) %>%
#  mutate(sector_lower_case=tolower(sector_code))
 # select(-category)

#sectors <- edgar_GHG %>% select(sector_code) %>% distinct() %>% mutate(sector_lower_case=tolower(sector_code))

#uhoh <- anti_join(crf_data,sectors,by="sector_lower_case",ignore_case=TRUE)

crf_total_sector<-crf_data %>%
  filter(category=="IPC0")

match<-openxlsx::read.xlsx("C:/Users/doen/Documents/IPCC data/PRIMAP_gases_matching.xlsx") %>%
  select(-Description.PRIMAP)

match$Code.PRIMAP<-gsub(" ","",match$Code.PRIMAP)

gwps<-left_join(match,gwps,by=c("Code.gwps"="gas")) %>% #CH4 will be a problem here
  select(Code.PRIMAP,Code.gwps,gwp_ar6)
gwps2<-gwps %>% 
  mutate(Code.gwps=ifelse(Code.PRIMAP=="NF3","NF3", Code.gwps)) %>%  #NF3 was not included in the gwps dataset, but it was listed in the list we received from WG I
  mutate(gwp_ar6=ifelse(Code.PRIMAP=="NF3",17162, gwp_ar6))

crf_total_sector<-left_join(crf_total_sector,gwps,by=c("entity"="Code.PRIMAP"))
crf_total_sector<-crf_total_sector %>%
  filter(unit=="kt") %>%
  filter(country!="EU27BX") %>%
  filter(country!="EU28")

#crf_total_sector<-crf_total_sector %>%
#  mutate_at(.vars = vars("1986":"2018"),.funs = list(gwp=~.*gwp_ar6))

anti_join(unique(crf_total_sector[,"country"]),as_tibble(unique(edgar_GHG[,"ISO"])),by=c("country"="value"))
          #LIE: Liechtenstein
          #MCO: Monaco
#unique(edgar_GHG[,c("ISO","country")])
#Liechtenstein and Monaco are not in the EDGAR database

# crf_total_sector<-gather(crf_total_sector,key = "year", value = "gwp_ar6", "1986_gwp":"2018_gwp") %>%
#   select(-c("unit":"2018")) %>%
#   mutate(year=as.numeric(gsub("_gwp","",year)))

crf_total_sector<-gather(crf_total_sector,key = "year", value = "value", "1986":"2018") %>%
  mutate(value=value/1e6) %>%
  mutate(unit="Gt") %>%
  mutate(year=as.numeric(year))

crf_total_sector<-crf_total_sector %>% filter(!is.na(Code.gwps))

#### prepare EDGAR data for comparison
edgar_data<-edgar_GHG

#methane <- edgar_data %>% 
#  filter(!is.na(CH4)) 

edgar_data<-edgar_data %>%
  group_by(year,ISO,country) %>%
  summarise_at(vars(CO2:SF6),sum,na.rm=TRUE) %>% 
  mutate_at(vars(CO2:SF6),list(~./1e9))

edgar_data <- gather(edgar_data,gas,value,CO2:SF6)

#edgar_data <- left_join(edgar_data,gwps,by = c("gas"="Code.gwps")) %>%
#  mutate(gwp_ar6=gwp_ar6*value) %>%
#  select(-value)

# recalculate methane in AR6 using biogenic / non-biogenic methane GWPs
# methane <- methane %>% 
#   group_by(year,ISO,country,sector_code) %>% 
#   summarise(CH4=sum(CH4,na.rm=TRUE))
# 
# methane <- left_join(methane,gwps_ch4,by = "sector_code")
# 
# methane <- methane %>% 
#   mutate(CH4_adjusted=CH4*value) %>%
#   group_by(year,gas,ISO,country) %>%
#   summarise(CH4_adjusted=sum(CH4_adjusted)/1e9) %>% 
#   mutate(gas="CH4")
# 
# edgar_data <- left_join(edgar_data,methane,by = c("year", "gas","ISO","country"))
# 
# edgar_data <- edgar_data %>% 
#   mutate(gwp_ar6=ifelse(!is.na(CH4_adjusted),CH4_adjusted,gwp_ar6)) %>% 
#   select(-CH4_adjusted)

### comparison
#edgar_data %>% filter(gas!="CH4") %>% filter(ISO %in% crf_total_sector$country)

combined_crf_edgar <- left_join(crf_total_sector,edgar_data,by=c("country"="ISO","year"="year","Code.gwps"="gas")) %>%
  rename("GHG_crf"="value.x","GHG_edgar"="value.y")

#combined_crf_edgar <- combined_crf_edgar %>%
#  filter(year==2018) %>%
#  filter(!is.na(GHG_crf))

combined_crf_edgar <- combined_crf_edgar %>%
  mutate(diff=GHG_edgar-GHG_crf) %>%
  mutate(diff_p=(GHG_edgar-GHG_crf)/GHG_edgar*100)


plot_diff<-function(gas){
  dat<- combined_crf_edgar %>%
    filter(year==2018) %>%
    filter(Code.gwps==gas)
  
  dat<-gather(dat,key="data",value = "value", c("GHG_edgar","GHG_crf"))
  p<-ggplot(dat, aes(x=country,y=value,fill=data)) + geom_bar(stat = "identity", position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

p1<-plot_diff("CO2")
p2<-plot_diff("CH4")
p3<-plot_diff("N2O")

box_diff<-function(gas){
  dat<- combined_crf_edgar %>%
    filter(Code.gwps==gas)
  
  #dat<-gather(dat,key="data",value = "value", c("GHG_edgar","GHG_crf"))
  p<-ggplot(dat, aes(x=country,goup=country,y=diff_p)) + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

p4<-box_diff("CO2")
# CO2<- combined_crf_edgar %>%
#   filter(Code.gwps=="CO2")
# 
# CO2<-gather(CO2,key="data",value = "value", c("GHG_edgar","GHG_crf"))
# ggplot(CO2, aes(x=country,y=value,fill=data)) + geom_bar(stat = "identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# CH4<- combined_crf_edgar %>%
#   filter(Code.gwps=="CH4")
# 
# CH4<-gather(CH4,key="data",value = "value", c("GHG_edgar","GHG_crf"))
# ggplot(CH4, aes(x=country,y=value,fill=data)) + geom_bar(stat = "identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# N2O<- combined_crf_edgar %>%
#   filter(Code.gwps=="N2O")
# 
# N2O<-gather(N2O,key="data",value = "value", c("GHG_edgar","GHG_crf"))
# ggplot(N2O, aes(x=country,y=value,fill=data)) + geom_bar(stat = "identity", position=position_dodge()) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
