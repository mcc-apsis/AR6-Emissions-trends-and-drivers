
rm(list = ls())
library(tidyverse)
library(openxlsx)










#load('Data/edgar_data_gwp_ar5.RData')

# iea <- openxlsx::read.xlsx('Data/IEA/wbig.xlsx')
# iea <- read.csv(file="C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/IEA backup/World_Energy_Balances_World_2017_edition/wbig.csv",sep = ";",header = TRUE)
# iea <- iea %>% 
#   rename_at(vars(names(iea)),~c("country","product","flow",1960:2016)) %>% 
#   filter(country!="COUNTRY")
# iea <- gather(iea,year,value,`1960`:`2016`)
# iea <- iea %>% 
#   filter(year>1969) %>%
#   mutate(value=ifelse(value=="..",NA,value)) %>% 
#   mutate(value=ifelse(value=="x",NA,value))
# 
# iea <- iea %>% 
#   mutate(value=as.numeric(value))
 
# names <- openxlsx::read.xlsx('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx',sheet = 'alternative_names')
# iea <- left_join(iea %>% mutate(country=tolower(country)),names,by=c("country"="alternative.name"))

#save(iea,file="Data/iea_web.RData")

#load("Data/iea_web.RData")

#flows <- iea %>% select(flow) %>% distinct()

flows <- openxlsx::read.xlsx('Data/Codes and classifications/IEA_checking_will.xlsx') %>% 
  select(FLOW:Total,Signs)


class <- openxlsx::read.xlsx("Data/Codes and classifications/iea_edgar_sector_classification.xlsx")
class <- class %>% 
  rename_at(vars(names(class)),~c(names(class)[1:5],1:8))
class <- gather(class,neg,iea_code,`1`:`8`)
class <- class %>% 
  filter(!is.na(iea_code)) %>% 
  select(-neg) %>% 
  select(-ipcc_1996)


flows <- left_join(flows,class,by=c("CODE"="iea_code"))


openxlsx::write.xlsx(flows,'Data/Codes and classifications/iea_ipcc_sector_classification.xlsx')

iea <- iea %>%
  filter(year>1969) %>%
  mutate(value=ifelse(value=="..",NA,value)) %>%
  mutate(value=ifelse(value=="x",NA,value)) %>%
  mutate(value=as.numeric(value))


products <- iea %>% select(product) %>% distinct()


# data is huge. first sum to flows
countries <- iea %>% 
  filter(product=="TOTAL") %>% 
  group_by(country,year,flow) %>% 
  summarise(value=sum(value,na.rm=TRUE))

# remove non-countries
countries <- countries %>%
  ungroup() %>% 
  mutate(country=tolower(country)) %>% 
  filter(country!="world") %>%
  filter(country!="oecd americas") %>%
  filter(country!="non-oecd americas") %>%
  filter(country!="oecd asia oceania") %>%
  filter(country!="oecd europe") %>%
  filter(country!="africa") %>%
  filter(country!="middle east") %>%
  filter(country!="non-oecd europe and eurasia") %>%
  filter(country!="non-oecd asia (excluding china)") %>%
  filter(country!="china (p.r. of china and hong kong, china)") %>%
  filter(!grepl("memo",country)) %>%
  filter(year<2016)


### get the edgar/iea sector correspondence

class <- openxlsx::read.xlsx("Data/Codes and classifications/iea_edgar_sector_classification.xlsx")
class <- class %>% 
  rename_at(vars(names(class)),~c(names(class)[1:5],1:8))
class <- gather(class,neg,iea_code,`1`:`8`)
class <- class %>% 
  filter(!is.na(iea_code)) %>% 
  select(-neg) %>% 
  select(-ipcc_1996)

### join JRC flow correspondence
countries <- left_join(countries,class %>% select(-code,-description) %>% distinct(),by=c("flow"="iea_code"))
countries <- countries %>% 
  mutate(sector_chapter=ifelse(is.na(sector_chapter),1,sector_chapter)) %>% 
  mutate(chapter_title=ifelse(sector_chapter==1,flow,chapter_title))

totals <- countries %>% 
  group_by(sector_chapter,year) %>% 
  summarise(value=sum(value,na.rm=TRUE))

totals  %>% 
  filter(sector_chapter!=1) %>% 
  filter(year>1970) %>% 
  mutate(year=as.numeric(year)) %>% 
  ggplot(.,aes(x=year,y=value,fill=chapter_title)) +
  geom_area(colour="#737373")

blarg <- totals %>% 
  filter(year==2015) %>% 
  arrange(desc(value))


wtf <- countries %>% 
  select(flow,sector_chapter,chapter_title) %>% 
  distinct()

countries <- countries %>% 
  ungroup() %>% 
  mutate(sector_chapter=ifelse(flow=="NONENUSE",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="TPATFUEL",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="TBLASTFUR",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="TCHARCOAL",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="ONONSPEC",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="EGTL",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="TRANSFER",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="STATDIFF",2,sector_chapter)) %>% 
  mutate(sector_chapter=ifelse(flow=="INDPROD",1,sector_chapter)) %>% ### ???
  mutate(sector_chapter=ifelse(flow=="EXPORTS",1,sector_chapter))
  
grand_totals <- countries %>% 
  filter(sector_chapter!=1) %>% 
  group_by(year) %>% 
  summarise(value=sum(value,na.rm=TRUE))

tpes <- iea %>% 
  filter(year==2015) %>% 
  filter(country=="World")

blarg <- iea %>% select(flow) %>% distinct()

tpes <- tpes %>% 
  filter(flow=="TPES")

### what flows arent joined?
notjoined <- countries %>% 
  filter(sector_chapter==1) %>% 
  filter(year==2015) %>% 
  select(flow) %>%
  distinct()

iea_labels <- openxlsx::read.xlsx("Data/IEA/iea_labels.xlsx")
notjoined <- left_join(notjoined,iea_labels,by=c("flow"="flow_label_short"))
class <- left_join(class,iea_labels,by=c("iea_code"="flow_label_short"))


wb <- openxlsx::loadWorkbook("Data/Codes and classifications/iea_edgar_sector_classification.xlsx")
openxlsx::addWorksheet(wb,"Not joined")
openxlsx::writeData(wb, sheet = "Not joined", notjoined, colNames = T, rowNames = F)
openxlsx::saveWorkbook(wb,"Data/Codes and classifications/iea_edgar_sector_classification.xlsx",overwrite=T)

########## WHAT ABOUT THE SHORT BALANCES VERSION

ieabal <- openxlsx::read.xlsx('Data/IEA/wbal.xlsx')

ieabal <- read.csv(file="C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/IEA backup/World_Energy_Balances_World_2017_edition/wbig.csv",sep = ";",header = TRUE)
ieabal <- ieabal %>%
  rename_at(vars(names(ieabal)),~c("country","flow","product",1960:2016)) %>%
  filter(country!="COUNTRY")

ieabal <- gather(ieabal,year,value,`1960`:`2016`)
ieabal <- ieabal %>%
  filter(year>1969) %>%
  mutate(value=ifelse(value=="..",NA,value)) %>%
  mutate(value=ifelse(value=="x",NA,value)) %>%
  mutate(value=as.numeric(value))


products <- ieabal %>% select(product) %>% distinct()
flows <- ieabal %>% select(flow) %>% distinct()

# data is huge. first sum to flows
countries <- ieabal %>% 
  filter(product!="TOTAL") %>% 
  group_by(country,year,flow) %>% 
  summarise(value=sum(value,na.rm=TRUE))


# remove non-countries
countries <- countries %>%
  ungroup() %>% 
  mutate(country=tolower(country)) %>% 
  filter(country!="world") %>%
  filter(country!="oecd americas") %>%
  filter(country!="non-oecd americas") %>%
  filter(country!="oecd asia oceania") %>%
  filter(country!="oecd europe") %>%
  filter(country!="africa") %>%
  filter(country!="middle east") %>%
  filter(country!="non-oecd europe and eurasia") %>%
  filter(country!="non-oecd asia (excluding china)") %>%
  filter(country!="china (p.r. of china and hong kong, china)") %>%
  filter(!grepl("memo",country)) %>%
  filter(year<2016)

### join JRC flow correspondence
countries <- left_join(countries,class %>% select(-code,-description) %>% distinct(),by=c("flow"="iea_code"))
countries <- countries %>% 
  mutate(sector_chapter=ifelse(is.na(sector_chapter),1,sector_chapter)) %>% 
  mutate(chapter_title=ifelse(sector_chapter==1,"unallocated",chapter_title))



baltotals <- countries %>% 
  group_by(sector_chapter,chapter_title,year) %>% 
  summarise(value=sum(value,na.rm=TRUE))

baltotals  %>% 
  filter(sector_chapter!=1) %>% 
  mutate(year=as.numeric(year)) %>% 
  ggplot(.,aes(x=year,y=value,fill=chapter_title)) +
  geom_area(colour="#737373")

grand_totals <- baltotals %>% 
  filter(sector_chapter!=1) %>% 
  group_by(year) %>% 
  summarise(value=sum(value,na.rm=TRUE))
