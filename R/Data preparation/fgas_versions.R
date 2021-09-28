

rm(list = ls())
library(tidyverse)
load('Data/edgar6_v4_data_raw.RData')

fgas_v6 <- edgar_raw %>% 
  filter(gas!="CO2") %>% 
  filter(gas!="CH4") %>% 
  filter(gas!="N2O") %>% 
  group_by(year,gas,gwp100_ar4,gwp100_ar6) %>% 
  summarise(edgar_v6=sum(value,na.rm=TRUE))


#########

fgas_v5 <- openxlsx::read.xlsx('Data/EDGAR/fgas versions/f-gases_booklet2019.xlsx')

fgas_v5 <- gather(fgas_v5,year,value,Y_1970:Y_2015)
fgas_v5$year <- gsub("Y_","",fgas_v5$year)
fgas_v5$value <- gsub("NULL",NA,fgas_v5$value)
fgas_v5 <- fgas_v5 %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(year=as.numeric(year))

fgas_v5 <- fgas_v5 %>% 
  group_by(Substance,year) %>% 
  summarise(edgar_v5=sum(value,na.rm=TRUE)*1000) %>% 
  select(gas=Substance,year,edgar_v5)

fgas_v6 <- full_join(fgas_v6,fgas_v5,by = c("year", "gas"))
not_joined <- anti_join(fgas_v5,fgas_v6,by = c("year", "gas"))

#########


fgas_v4 <- openxlsx::read.xlsx('Data/EDGAR/fgas versions/f-gases_v432.xlsx')

fgas_v4 <- gather(fgas_v4,year,value,Y_1970:Y_2015)
fgas_v4$year <- gsub("Y_","",fgas_v4$year)
fgas_v4$value <- gsub("NULL",NA,fgas_v4$value)
fgas_v4 <- fgas_v4 %>% 
  mutate(value=as.numeric(value)) %>% 
  mutate(year=as.numeric(year))


fgas_v4 <- fgas_v4 %>% 
  group_by(Substance,year) %>% 
  summarise(edgar_v4.3.2=sum(value,na.rm=TRUE)*1000) %>% 
  select(gas=Substance,year,edgar_v4.3.2)


fgas_v6 <- full_join(fgas_v6,fgas_v4,by = c("year", "gas"))
not_joined <- anti_join(fgas_v4,fgas_v6,by = c("year", "gas"))

#########


fgas_v5FT <- openxlsx::read.xlsx('Data/EDGAR/fgas versions/EDGAR v5.0 FT2019, Part C- F-gases (by PBL).xlsx',sheet=3,startRow = 10)
fgas_v5FT <- fgas_v5FT[1:59]
fgas_v5FT <- gather(fgas_v5FT,year,value,'1970':'2019')

fgas_v5FT <- fgas_v5FT %>% 
  group_by(Fgas,year) %>% 
  summarise(edgar_v5FT=sum(value,na.rm=TRUE)*1000) %>% 
  select(gas=Fgas,year,edgar_v5FT) %>% 
  mutate(year=as.numeric(year))

fgas_v6 <- full_join(fgas_v6,fgas_v5FT,by = c("year", "gas"))
not_joined <- anti_join(fgas_v5FT,fgas_v6,by = c("year", "gas"))

#########

fgas <- fgas_v6 %>% 
  mutate(gwp100_ar4=ifelse(gas=="C7F16",9300,gwp100_ar4)) %>% 
  mutate(gwp100_ar6=ifelse(gas=="C7F16",8410,gwp100_ar6))


#########

fgas <- fgas %>% 
  mutate(group=ifelse(grepl("HFC",gas),"HFCs","PFCs")) %>% 
  mutate(group=ifelse(gas=="NF3",NA,group)) %>% 
  mutate(group=ifelse(grepl("HCFC",gas),NA,group)) %>% 
  mutate(group=ifelse(gas=="SF6","SF6",group))


write.xlsx(fgas,'edgar_fgas_comparison.xlsx')


fgas <- gather(fgas,version,value,edgar_v6,edgar_v5,edgar_v4.3.2,edgar_v5FT)
fgas <- fgas %>% 
  mutate(value=value*gwp100_ar6) %>% 
  group_by(year,version) %>% 
  summarise(value=sum(value,na.rm=TRUE))


fgas %>% ggplot(.,aes(y=value,x=year,colour=version,group=version)) +
  geom_path()

