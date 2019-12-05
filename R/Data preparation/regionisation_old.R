rm(list = ls())
library(zoo)
library(openxlsx)
library(tidyverse)


tsu_codes <- read.xlsx('Data/Country categories plus alpha codes 11-11-2019.xlsx','Regional breakdown',startRow = 10) %>% 
  select(region_ar6_5='High.Level.(5)',region_ar6_10="Intermediate.level.(10)",region_ar6_22="Low.Level.(22)",ISO="https://www.iban.com/country-codes")


tsu_codes <- tsu_codes %>% 
  filter(!is.na(ISO)) %>% 
  mutate(region_ar6_5=na.locf(region_ar6_5,na.rm=FALSE)) %>% 
  mutate(region_ar6_10=na.locf(region_ar6_10,na.rm=FALSE))


tsu_codes <- tsu_codes %>% 
  mutate(ISO=as.character(ISO)) %>% 
  separate(ISO,into=paste(1:30),sep=",")

tsu_codes <- gather(tsu_codes,value,ISO,-region_ar6_5,-region_ar6_10,-region_ar6_22)

tsu_codes <- tsu_codes %>% 
  filter(!is.na(ISO)) %>% 
  select(-value) %>% 
  select(ISO,everything())

tsu_codes$ISO = gsub(" ","",x = tsu_codes$ISO)

codes <- read.xlsx('Data/ISOcodes.xlsx',sheet = 'ISO_master')

tsu_codes <- left_join(tsu_codes,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))
missing <- anti_join(tsu_codes,codes %>% select(name,alpha.3),by=c("ISO"="alpha.3"))

tsu_codes <- tsu_codes %>% 
  select(ISO,name,everything()) %>% 
  arrange(ISO)

tsu_dev <- read.xlsx('Data/Country categories plus alpha codes 11-11-2019.xlsx','Development level ',startRow = 2) %>% 
  select(developed = "Developed.Alpha.-.3",developing="Developing.Alpha-3",ldc="LDC./.Alpha-3")

blarg <- tsu_dev %>% 
  separate(developed,into=paste("developed",1:35,sep="_"),sep=",") 
developed <- gather(blarg,key,ISO,developed_1:developed_35) %>% 
  select(ISO) %>% 
  filter(!is.na(ISO)) %>% 
  mutate(region_ar6_dev="developed")

blarg <- tsu_dev %>% 
  separate(developing,into=paste("developing",1:35,sep="_"),sep=",") 
developing <- gather(blarg,key,ISO,developing_1:developing_35) %>% 
  select(ISO) %>% 
  filter(!is.na(ISO)) %>% 
  mutate(region_ar6_dev="developing")

blarg <- tsu_dev %>% 
  separate(ldc,into=paste("ldc",1:35,sep="_"),sep=",") 
ldc <- gather(blarg,key,ISO,ldc_1:ldc_35) %>% 
  select(ISO) %>% 
  filter(!is.na(ISO)) %>% 
  mutate(region_ar6_dev="ldc")

z <- rbind(developed,developing,ldc)

tsu_codes <- left_join(tsu_codes,z,by=c("ISO"="ISO"))

load('Output/edgar_data_gwp_ar5.RData')

edgar_GHG_ar5 <- edgar_GHG_ar5 %>% 
  select(ISO,country) %>% 
  unique()

tsu_codes <- left_join(tsu_codes,edgar_GHG_ar5,by=("ISO"="ISO"))
tsu_codes <- tsu_codes %>% 
  mutate(name=ifelse(is.na(name),country,name)) %>% 
  select(-country)


####### short region names ##########

tsu_codes <- tsu_codes %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Developed Countries","DEV",NA)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Latin America and Caribbean","LAM",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Africa and Middle East","AME",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Eastern Europe and West-Central Asia","EEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Asia and developing Pacific","APC",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Shipping","SEA",region_ar6_5_short)) %>% 
  mutate(region_ar6_5_short=ifelse(region_ar6_5=="Intl. Aviation","AIR",region_ar6_5_short))


# library(ggmap)
# library(maps)
# library(mapdata)
# 
# world <- map_data("world") %>%
#   filter(region != "Antarctica") %>%
#   mutate(region=tolower(region))
# 
# 
# isos <- read.xlsx(file="C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/R/.Place names and codes/output/ISOcodes.xlsx",sheetName="alternative_names",header=TRUE) %>% 
#   select(country=alternative.name,ISO=alpha.3)
# 
# world <- left_join(world,isos,by=c("region"="country"))
# 
# world <- left_join(world,tsu_codes,by=c("ISO"="ISO"))
# 
# world %>%
#   ggplot(.) +
#   geom_polygon(aes(long,lat,group = paste(region,group),fill=region_ar6_5),color="white") +
#   theme_void() +
#   coord_fixed(1) +
#   scale_fill_discrete() +
#   labs(title="AR6 5 regions")
# 
# world %>%
#   ggplot(.) +
#   geom_polygon(aes(long,lat,group = paste(region,group),fill=region_ar6_10),color="white") +
#   theme_void() +
#   coord_fixed(1) +
#   scale_fill_discrete() +
#   labs(title="AR6 10 regions")
# 
# world %>%
#   ggplot(.) +
#   geom_polygon(aes(long,lat,group = paste(region,group),fill=region_ar6_dev),color="white") +
#   theme_void() +
#   coord_fixed(1) +
#   scale_fill_discrete() +
#   labs(title="AR6 income regions")

write.xlsx(tsu_codes,file="Output/IPCC_regions.xlsx",sheetName="regions",row.names = FALSE)

save(tsu_codes,file='Data/tsu_codes.RData')
