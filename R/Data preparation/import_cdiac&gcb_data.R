library(tidyverse)
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set2") + scale_fill_brewer(palette="Set2")


#CDIAC
CDIAC<-read_csv('../../Data/supplemetary data/CDIAC_by_country_1751-2014.csv',na=".")
CDIAC<-CDIAC[-(1:3),]

cdiac_countries<-unique(CDIAC$Nation)
load("../../Data/ipcc_regions.RData")

cdiac_countries<-as.data.frame(cbind(cdiac_countries,tolower(cdiac_countries)))
ipcc_regions<-ipcc_regions %>%
  mutate(country_name=tolower(name))

#cdiac_countries<-cbind(cdiac_countries,NA)
cdiac_regions<-full_join(ipcc_regions,cdiac_countries,by=c("country_name"="V2"))


without_regions<-cdiac_regions %>%
  filter(is.na(region_ar6_6))

missing_countries<-cdiac_regions %>%
  filter(is.na(cdiac_countries))

cdiac_countries<-cdiac_countries %>%
  mutate(V2=ifelse(V2=="bahamas","bahamas, the",V2)) %>%
  mutate(V2=ifelse(V2=="antigua & barbuda","antigua and barbuda",V2)) %>%
  mutate(V2=ifelse(V2=="bonaire, saint eustatius, and saba","bonaire, sint eustatius and saba",V2)) %>%
  mutate(V2=ifelse(V2=="bosnia & herzegovina","bosnia and herzegovina",V2)) %>%
  mutate(V2=ifelse(V2=="brunei (darussalam)","brunei darussalam",V2)) %>%
  mutate(V2=ifelse(V2=="cape verde","cabo verde",V2)) %>%
  mutate(V2=ifelse(V2=="china (mainland)","china",V2)) %>%
  mutate(V2=ifelse(V2=="democratic republic of the congo (formerly zaire)","congo, dem. rep.",V2)) %>%
  mutate(V2=ifelse(V2=="congo","congo, rep.",V2)) %>%
  mutate(V2=ifelse(V2=="cote d ivoire","côte d'ivoire",V2)) %>%
  mutate(V2=ifelse(V2=="curacao","curaçao",V2)) %>%
  mutate(V2=ifelse(V2=="democratic people s republic of korea","korea, dem. people's rep.",V2)) %>%
  mutate(V2=ifelse(V2=="democratic republic of vietnam","vietnam",V2)) %>%
  mutate(V2=ifelse(V2=="egypt","egypt, arab rep.",V2)) %>%
  mutate(V2=ifelse(V2=="faeroe islands","faroe islands",V2)) %>%
  #mutate(V2=ifelse(V2=="antarctic fisheries","",V2)) %>%
  mutate(V2=ifelse(V2=="czechoslovakia","czech republic",V2)) %>%                                  
  mutate(V2=ifelse(V2=="east & west pakistan","bangladesh",V2)) %>%                            
  mutate(V2=ifelse(V2=="federal republic of germany","germany",V2)) %>%                     
  mutate(V2=ifelse(V2=="federated states of micronesia"          ,"micronesia, fed. sts.",V2)) %>%        
  mutate(V2=ifelse(V2=="federation of malaya-singapore"                 ,"singapore",V2)) %>% 
  mutate(V2=ifelse(V2=="former democratic yemen"                        ,"yemen",V2)) %>% 
  mutate(V2=ifelse(V2=="former german democratic republic"              ,"germany",V2)) %>% 
  mutate(V2=ifelse(V2=="former panama canal zone"                       ,"united states",V2)) %>% 
  mutate(V2=ifelse(V2=="former yemen"                                   ,"yemen",V2)) %>% 
  mutate(V2=ifelse(V2=="france (including monaco)"                      ,"france",V2)) %>% 
  mutate(V2=ifelse(V2=="french equatorial africa"                       ,"chad",V2)) %>% 
  mutate(V2=ifelse(V2=="french indo-china"                              ,"cambodia",V2)) %>% 
  mutate(V2=ifelse(V2=="french west africa"                             ,"niger",V2)) %>% 
  mutate(V2=ifelse(V2=="gambia"                                         ,"gambia, the",V2)) %>% 
  mutate(V2=ifelse(V2=="guinea bissau"                                  ,"guinea-bissau",V2)) %>% 
  mutate(V2=ifelse(V2=="hong kong special adminstrative region of china","hong kong sar, china",V2)) %>% 
  mutate(V2=ifelse(V2=="islamic republic of iran"                       ,"iran, islamic rep.",V2)) %>% 
  mutate(V2=ifelse(V2=="italy (including san marino)"                   ,"italy",V2)) %>% 
  mutate(V2=ifelse(V2=="japan (excluding the ruyuku islands)"           ,"japan",V2)) %>% 
  mutate(V2=ifelse(V2=="kuwaiti oil fires"                              ,"kuwait",V2)) %>% 
  mutate(V2=ifelse(V2=="kyrgyzstan"                                     ,"kyrgyz republic",V2)) %>% 
  mutate(V2=ifelse(V2=="lao people s democratic republic"               ,"lao pdr",V2)) %>% 
  mutate(V2=ifelse(V2=="leeward islands"                                ,"guadeloupe",V2)) %>% 
  mutate(V2=ifelse(V2=="libyan arab jamahiriyah"                        ,"libya",V2)) %>% 
  mutate(V2=ifelse(V2=="macau special adminstrative region of china"    ,"macao sar, china",V2)) %>% 
  mutate(V2=ifelse(V2=="macedonia"                                      ,"north macedonia",V2)) %>% 
  mutate(V2=ifelse(V2=="myanmar (formerly burma)"                       ,"myanmar",V2)) %>% 
  mutate(V2=ifelse(V2=="netherland antilles"                            ,"aruba",V2)) %>% 
  mutate(V2=ifelse(V2=="netherland antilles and aruba"                  ,"aruba",V2)) %>% 
  mutate(V2=ifelse(V2=="occupied palestinian territory"                 ,"jordan",V2)) %>% 
  mutate(V2=ifelse(V2=="pacific islands (palau)"                        ,"palau",V2)) %>% 
  mutate(V2=ifelse(V2=="peninsular malaysia"                            ,"malaysia",V2)) %>% 
  mutate(V2=ifelse(V2=="plurinational state of bolivia"                 ,"bolivia",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of cameroon"                           ,"cameroon",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of korea"                              ,"korea, rep.",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of moldova"                            ,"moldova",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of south sudan"                        ,"south sudan",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of south vietnam"                      ,"vietnam",V2)) %>% 
  mutate(V2=ifelse(V2=="republic of sudan"                              ,"sudan",V2)) %>% 
  mutate(V2=ifelse(V2=="reunion"                                        ,"réunion",V2)) %>% 
  mutate(V2=ifelse(V2=="rhodesia-nyasaland"                             ,"malawi",V2)) %>% 
  mutate(V2=ifelse(V2=="rwanda-urundi"                                  ,"burundi",V2)) %>% 
  mutate(V2=ifelse(V2=="ryukyu islands"                                 ,"japan",V2)) %>% 
  mutate(V2=ifelse(V2=="sabah"                                          ,"malaysia",V2)) %>% 
  mutate(V2=ifelse(V2=="saint helena"                                   ,"united kingdom",V2)) %>% 
  mutate(V2=ifelse(V2=="saint lucia"                                    ,"st. lucia",V2)) %>% 
  mutate(V2=ifelse(V2=="saint martin (dutch portion)"                   ,"st. martin (french part)",V2)) %>% 
  mutate(V2=ifelse(V2=="sao tome & principe"                            ,"são tomé and principe",V2)) %>% 
  mutate(V2=ifelse(V2=="sarawak"                                        ,"malaysia",V2)) %>% 
  mutate(V2=ifelse(V2=="slovakia"                                       ,"slovak republic",V2)) %>% 
  mutate(V2=ifelse(V2=="st. kitts-nevis"                                ,"st. kitts and nevis",V2)) %>% 
  mutate(V2=ifelse(V2=="st. kitts-nevis-anguilla"                       ,"st. kitts and nevis",V2)) %>% 
  mutate(V2=ifelse(V2=="st. pierre & miquelon"                          ,"france",V2)) %>% 
  mutate(V2=ifelse(V2=="st. vincent & the grenadines"                   ,"st. vincent and the grenadines",V2)) %>% 
  mutate(V2=ifelse(V2=="taiwan"                                         ,"taiwan, china",V2)) %>% 
  mutate(V2=ifelse(V2=="tanganyika"                                     ,"tanzania",V2)) %>% 
  mutate(V2=ifelse(V2=="timor-leste (formerly east timor)"              ,"timor-leste",V2)) %>% 
  mutate(V2=ifelse(V2=="united korea"                                   ,"korea, rep.",V2)) %>% 
  mutate(V2=ifelse(V2=="united republic of tanzania"                    ,"tanzania",V2)) %>% 
  mutate(V2=ifelse(V2=="united states of america"                       ,"united states",V2)) %>% 
  #mutate(V2=ifelse(V2=="ussr"                                           ,"",V2)) %>% 
  mutate(V2=ifelse(V2=="venezuela"                                      ,"venezuela, rb",V2)) %>% 
  mutate(V2=ifelse(V2=="viet nam"                                       ,"vietnam",V2)) %>% 
  mutate(V2=ifelse(V2=="wallis and futuna islands"                      ,"wallis and futuna",V2)) %>% 
  mutate(V2=ifelse(V2=="yemen"                                          ,"yemen, rep.",V2))%>%
  #mutate(V2=ifelse(V2=="yugoslavia (former socialist federal republic)" ,"",V2)) %>% 
  mutate(V2=ifelse(V2=="yugoslavia (montenegro & serbia)"               ,"serbia",V2)) %>% 
  mutate(V2=ifelse(V2=="zanzibar" ,"tanzania",V2)) 

  
  

cdiac_regions<-full_join(ipcc_regions,cdiac_countries,by=c("country_name"="V2"))


##GCB
GCB<- openxlsx::read.xlsx('../../Data/supplemetary data/GCB_National_Carbon_Emissions_2020v1.0.xlsx',
                                      sheet='Territorial Emissions',startRow=12,na.strings = "NaN")
colnames(GCB)[1]<-"year"
gcb_countries<-as.data.frame(cbind(colnames(GCB),gsub("\\."," ",tolower(colnames(GCB)))))
gcb_countries<-gcb_countries[2:218,]
                             
gcb_regions<-full_join(ipcc_regions,gcb_countries,by=c("country_name"="V2"))


without_regions<-gcb_regions %>%
  filter(is.na(region_ar6_6))

missing_countries<-gcb_regions %>%
  filter(is.na(V1))

gcb_countries<-gcb_countries %>%
  mutate(V2=ifelse(V2=="bahamas","bahamas, the",V2)) %>%
  mutate(V2=ifelse(V2=="bonaire, saint eustatius and saba","bonaire, sint eustatius and saba",V2)) %>%
  mutate(V2=ifelse(V2=="cape verde","cabo verde",V2)) %>%
  mutate(V2=ifelse(V2=="democratic republic of the congo","congo, dem. rep.",V2)) %>%
  mutate(V2=ifelse(V2=="congo","congo, rep.",V2)) %>%
  mutate(V2=ifelse(V2=="north korea","korea, dem. people's rep.",V2)) %>%
  #mutate(V2=ifelse(V2=="democratic republic of vietnam","vietnam",V2)) %>%
  mutate(V2=ifelse(V2=="egypt","egypt, arab rep.",V2)) %>%
  mutate(V2=ifelse(V2=="faeroe islands","faroe islands",V2)) %>%
  mutate(V2=ifelse(V2=="kosovo","montenegro",V2)) %>%
  #mutate(V2=ifelse(V2=="antarctic fisheries","",V2)) %>%
  mutate(V2=ifelse(V2=="micronesia (federated states of)"          ,"micronesia, fed. sts.",V2)) %>%        
  mutate(V2=ifelse(V2=="gambia"                                         ,"gambia, the",V2)) %>% 
  mutate(V2=ifelse(V2=="hong kong","hong kong sar, china",V2)) %>% 
  mutate(V2=ifelse(V2=="iran"                       ,"iran, islamic rep.",V2)) %>% 
  mutate(V2=ifelse(V2=="kyrgyzstan"                                     ,"kyrgyz republic",V2)) %>% 
  mutate(V2=ifelse(V2=="laos"               ,"lao pdr",V2)) %>% 
  mutate(V2=ifelse(V2=="macao"    ,"macao sar, china",V2)) %>% 
  mutate(V2=ifelse(V2=="occupied palestinian territory"                 ,"jordan",V2)) %>% 
  mutate(V2=ifelse(V2=="south korea"                              ,"korea, rep.",V2)) %>% 
  mutate(V2=ifelse(V2=="saint helena"                                   ,"united kingdom",V2)) %>% 
  mutate(V2=ifelse(V2=="saint lucia"                                    ,"st. lucia",V2)) %>% 
  mutate(V2=ifelse(V2=="sao tome and principe"                            ,"são tomé and principe",V2)) %>% 
  mutate(V2=ifelse(V2=="slovakia"                                       ,"slovak republic",V2)) %>% 
  mutate(V2=ifelse(V2=="saint kitts and nevis"                                ,"st. kitts and nevis",V2)) %>% 
  mutate(V2=ifelse(V2=="saint vincent and the grenadines"                   ,"st. vincent and the grenadines",V2)) %>% 
  mutate(V2=ifelse(V2=="taiwan"                                         ,"taiwan, china",V2)) %>% 
  mutate(V2=ifelse(V2=="usa"                       ,"united states",V2)) %>% 
  #mutate(V2=ifelse(V2=="ussr"                                           ,"",V2)) %>% 
  mutate(V2=ifelse(V2=="venezuela"                                      ,"venezuela, rb",V2)) %>% 
  mutate(V2=ifelse(V2=="viet nam"                                       ,"vietnam",V2)) %>% 
  mutate(V2=ifelse(V2=="wallis and futuna islands"                      ,"wallis and futuna",V2)) %>% 
  mutate(V2=ifelse(V2=="yemen"                                          ,"yemen, rep.",V2)) %>%
  mutate(V2=ifelse(V2=="syria"                                          ,"syrian arab republic",V2))

gcb_regions<-full_join(ipcc_regions,gcb_countries,by=c("country_name"="V2"))

GCB<-gather(GCB[,1:218],country,ghg,Afghanistan:Zimbabwe)

GCB<-left_join(GCB,gcb_regions,by=c("country"="V1"))

GCB<-GCB %>%
  mutate(ghg=(ghg*3.664)/1e3) #the data provided by GCB are in MtC (1MtC = 1 million tonne of carbon = 3.664 million tonnes of CO2)


ussr_shares<-GCB %>%
  filter(country %in% c("Lithuania", "Georgia","Estonia" ,"Latvia" 	,"Ukraine" 	,"Belarus" 	,"Moldova","Kyrgyzstan" 	,"Uzbekistan" 	,"Tajikistan","Armenia" ,"Azerbaijan" ,"Turkmenistan","Russian.Federation" ,"Kazakhstan")) %>%
  filter(year==1959) %>%
  mutate(ghg_ussr=sum(ghg))%>%
  mutate(share=ghg/ghg_ussr)

ussr_shares<-ussr_shares %>%
  group_by(region_ar6_10)%>%
  summarise(share=sum(share))

yugoslavia_shares<-GCB %>%
  filter(country %in% c("Bosnia.and.Herzegovina",
                        "Croatia",
                        "Kosovo",
                        "Montenegro",
                        "North.Macedonia",
                        "Serbia",
                        "Slovenia")) %>%
  filter(year==1959) %>%
  mutate(ghg_yugo=sum(ghg,na.rm=TRUE))%>%
  mutate(share=ghg/ghg_yugo)

yugoslavia_shares<-yugoslavia_shares %>%
  group_by(region_ar6_10)%>%
  summarise(share=sum(share,na.rm = TRUE))

CDIAC<-CDIAC %>%
  select("country"="Nation","year"="Year","ghg"="Total CO2 emissions from fossil-fuels and cement production (thousand metric tons of C)")%>%
  mutate(ghg=(ghg*3.667)/1e6)%>%
  filter(year<1959)

CDIAC<-left_join(CDIAC,cdiac_regions,by=c("country"="cdiac_countries"))

time_series<-rbind(CDIAC,GCB)
hist_emissions_1850<-time_series %>%
  filter(year>=1850) %>%
  filter(country!="USSR") %>%
  filter(country!="YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)") %>%
  group_by(region_ar6_10) %>%
  summarise(ghg_cum_1850=sum(ghg,na.rm = TRUE))

hist_emissions_1750<-time_series %>%
  filter(year>=1750) %>%
  filter(country!="USSR") %>%
  filter(country!="YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)") %>%
  group_by(region_ar6_10) %>%
  summarise(ghg_cum_1750=sum(ghg,na.rm = TRUE))


###split USSR and Yugoslavia between Eurasia and Europe
hist_emissions_ussr_1750<-time_series %>%
  filter(country=="USSR") %>%
  filter(year>=1750) %>%
  summarise(ghg_cum_1750=sum(ghg,na.rm = TRUE))

ussr_shares<-ussr_shares %>%
  mutate(ghg_ussr_cum_1750=hist_emissions_ussr_1750$ghg_cum_1750)%>%
  mutate(ghg_ussr_cum_1750=share*ghg_ussr_cum_1750)

hist_emissions_ussr_1850<-time_series %>%
  filter(country=="USSR") %>%
  filter(year>=1850) %>%
  summarise(ghg_cum_1850=sum(ghg,na.rm = TRUE))

ussr_shares<-ussr_shares %>%
  mutate(ghg_ussr_cum_1850=hist_emissions_ussr_1850$ghg_cum_1850)%>%
  mutate(ghg_ussr_cum_1850=share*ghg_ussr_cum_1850)

hist_emissions_yugo_1850<-time_series %>%
  filter(country=="YUGOSLAVIA (FORMER SOCIALIST FEDERAL REPUBLIC)") %>%
  filter(year>=1850) %>%
  summarise(ghg_cum_1850=sum(ghg,na.rm = TRUE))

yugoslavia_shares<-yugoslavia_shares %>%
  mutate(ghg_yugo_cum_1850=hist_emissions_yugo_1850$ghg_cum_1850)%>%
  mutate(ghg_yugo_cum_1850=share*ghg_yugo_cum_1850)

hist_emissions_1750<-left_join(hist_emissions_1750,ussr_shares,by="region_ar6_10")
hist_emissions_1750<-left_join(hist_emissions_1750,yugoslavia_shares,by="region_ar6_10")

hist_emissions_1750<-hist_emissions_1750 %>%
  group_by(region_ar6_10) %>%
  mutate(ghg_cum_1750=sum(ghg_cum_1750,ghg_ussr_cum_1750,ghg_yugo_cum_1850,na.rm = TRUE)) %>%
  select(region_ar6_10,ghg_cum_1750)

hist_emissions_1850<-left_join(hist_emissions_1850,ussr_shares,by="region_ar6_10")
hist_emissions_1850<-left_join(hist_emissions_1850,yugoslavia_shares,by="region_ar6_10")

hist_emissions_1850<-hist_emissions_1850 %>%
  group_by(region_ar6_10) %>%
  mutate(ghg_cum_1850=sum(ghg_cum_1850,ghg_ussr_cum_1850,ghg_yugo_cum_1850,na.rm = TRUE)) %>%
  select(region_ar6_10,ghg_cum_1850)

hist_emissions_1850 %>%
  ggplot(aes(x="CO2-FFI emissions",y=ghg_cum_1850,fill=region_ar6_10))+
  geom_bar(stat="identity",position = "stack")+
  ylab('CO2 Emissions (Gt)') +
  xlab('') +
  ggtitle("Cumulative CO2 emissions 1850-2019")+
  theme(legend.title = element_blank())
