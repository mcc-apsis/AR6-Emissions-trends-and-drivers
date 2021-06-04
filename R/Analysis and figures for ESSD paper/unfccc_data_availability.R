library(readxl)
library(ggmap)
library(maps)
library(tidyverse)
library(scales)

unfccc <- read_excel("C:/Users/doen/ownCloud/AR6-Emissions-trends-and-drivers/R/Analysis and figures for ESSD paper/UNFCCC reports.xlsx")

summary(unfccc)
unfccc$country
class(unfccc$country)

class(unfccc$`submission date last BUR`)
class(unfccc$`last reporting year BUR`)
class(unfccc$`last reporting year NC`)
class(unfccc$`last reporting year NIR`)
class(unfccc$`annex-1`)
class(unfccc$`last reporting year BUR`)


unfccc$`last reporting year BUR`<-as.numeric(unfccc$`last reporting year BUR`)
unfccc$`last reporting year NC`<-as.numeric(unfccc$`last reporting year NC`)
unfccc$`last reporting year NIR`<-as.numeric(unfccc$`last reporting year NIR`)
#unfccc$`annex-1`<-as.logical(unfccc$`annex-1`)

unfccc <- unfccc %>%
  mutate(`last reporting year NC`=ifelse(country=="Nepal",2001,`last reporting year NC`)) %>%
  #mutate(`annex-1`=!is.na(`annex-1`)) %>%
  mutate(country=ifelse(country=="Democratic People's Republic of Korea","North Korea",country)) %>%
  mutate(country=ifelse(country=="Republic of Korea","South Korea",country)) %>%
  mutate(country=ifelse(country=="Russian Federation","Russia",country)) %>%
  mutate(country=ifelse(country=="United States","USA",country)) %>%
  mutate(country=ifelse(country=="United Kingdom","UK",country)) %>%
  mutate(country=ifelse(country=="Cote d'Ivoire","Ivory Coast",country)) %>%
  mutate(country=ifelse(country=="Chech Republic","Czechia",country)) #%>%
  #mutate(country=ifelse(country=="United Kingdom","UK",country)) %>%
  #mutate(country=ifelse(country=="United Kingdom","UK",country)) %>%


unfccc<-unfccc %>%
  mutate(last_reporting_year=pmax(`last reporting year BUR`,`last reporting year NC`,`last reporting year NIR`,na.rm=TRUE)) %>%
  rename(annex1=`annex-1`)

world <- map_data("world") %>% 
  filter(region!="Antarctica")
#world <- left_join(world %>% mutate(region=tolower(region)),isos,by=c("region"="alternative.name"))
world <- left_join(world,unfccc,by=c("region"="country"))

a<-world %>% filter(is.na(world$last_reporting_year)) %>% filter(is.na(`annex-1`))
unique(a$region)

p1 <- ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group=group, fill=last_reporting_year),color="grey",size=0.25,na.rm=T) + 
  #scale_fill_viridis_c(limits = c(0,35)) +
  scale_fill_steps(low = 'lightyellow',high = "darkblue", na.value="lightgrey", breaks=c(1990,1995,2000,2005,2010,2015,2020), oob=squish) +
  geom_polygon(data = world, aes(x=long, y = lat, group=group, fill=annex1),color="grey",size=0.25,na.rm=T,fill=c("green","red")) + 
  #scale_fill_discrete(c("green","yellow")) +
  #check again with new data the limit to 35 tCO2eq per capita
  theme(panel.border = element_rect(colour = "black",fill=NA),
        legend.position = c(0.5,0),
        legend.direction = "horizontal",
        legend.key.width = unit(2,"cm"),
        legend.key.height = unit(.2,"cm"),
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(0,0,0.5,0),units="cm"),
        plot.title = element_text(size=12),
        panel.grid=element_blank()) +
  ggtitle("Last UNFCCC reported data")
p1
