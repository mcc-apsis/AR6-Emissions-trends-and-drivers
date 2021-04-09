rm(list = ls())
library(tidyverse)
library(openxlsx)

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
  filter()

crf_total_sector[,as.character(1986:2018)]<-crf_total_sector[,as.character(1986:2018)]*as.vector(crf_total_sector[,"gwp_ar6"])

crf_total_sector<-crf_total_sector %>%
  mutate_at(.vars = vars("1986":"2018"),.funs = list(gwp=~.*gwp_ar6))
