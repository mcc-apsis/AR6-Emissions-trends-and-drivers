### wg2 regional stuff


```{r, plot_gases, include=FALSE, echo=FALSE }

plot_gas <- function(data,region) {
  
  
  gas_data <- data %>% 
    filter(region_ar6_10==region) %>% 
    filter(year>1989) %>% 
    filter(year<2019) %>% 
    group_by(year) %>% 
    summarise(CO2=sum(CO2,na.rm=T)/1e9,CH4=sum(CH4,na.rm=T)/1e9,N2O=sum(N2O,na.rm=T)/1e9,Fgas=sum(Fgas,na.rm=T)/1e9)
  
  gas_data <- gather(gas_data,gas,value,CO2:Fgas)
  
  
  
  shares <- gas_data %>% 
    filter(year %in% c(1990,2000,2010,2018)) %>% 
    group_by(year) %>% 
    mutate(totals=sum(value)) %>% 
    ungroup() %>% 
    group_by(year,gas) %>% 
    mutate(fractions=(value/totals)*100)
  
  shares <- locate_shares(shares,"gas",4)
  
  line_data <- data.frame(x=c(1990,1990,2000,2000,2010,2010,2018,2018),y=c(0,1.1*max(shares$totals)))
  
  
  
  
  p <- gas_data %>% 
    ggplot(.,aes(x=year,y=value,fill=gas)) +
    geom_area(colour="#737373") +
    
    geom_line(inherit.aes = FALSE,data=line_data,aes(x=x,y=y,group=x),alpha=0.3,linetype="dashed") +
    geom_vline(xintercept=c(1990,2000,2010,2018),alpha=0.3,linetype="dashed") +
    
    #### text with the shares
    geom_text(data=shares,aes(x=year+0.75,y=location,label=paste(round(fractions,0),"%",sep="")),size=3.5,colour="#252525")+
    
    #### text with the totals
    geom_text(data=shares %>% filter(gas=="Fgas"),aes(x=year,y=1.1*max(shares$totals),label=paste(round(totals,0),  "Gt")),size=3.5,colour="#252525")+
    
    ylab('GHG Emissions (GtCO2eq/yr)') +
    labs(fill="Gas") +
    #scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
    scale_x_continuous(breaks=c(1990,2000,2010,2018)) +
    big_trend_theme +
    theme(legend.position="bottom",
          legend.background = element_rect(linetype = 1, size = 0.5, colour = "#525252"),
          legend.margin = margin(l=5,t=5,b=5,r=10,unit="pt"),
          legend.text = element_text(size=8),
          legend.title = element_blank()) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE),
           shape = guide_legend(override.aes = list(size = 0.5)),
           color = guide_legend(override.aes = list(size = 0.5))) +
    ggtitle(paste0("a. ",region," GHG emissions by gas"))
  
  
  
  return(list("plot"=p,"data"=gas_data))
}



```



```{r plot_sectors, include=FALSE, echo=FALSE}

plot_sectors <- function(data,region) {
  
  #sectors
  sector_data <- data  %>% 
    filter(region_ar6_10==region) %>% 
    filter(year>1989) %>% 
    filter(year<2019) %>% 
    group_by(year,chapter,chapter_title) %>%  
    summarise(value=sum(GHG,na.rm=TRUE)/1e9) %>% 
    ungroup()
  
  sector_data$chapter_title <- as.factor(sector_data$chapter_title)
  sector_data$chapter_title <- factor(sector_data$chapter_title,levels(sector_data$chapter_title)[c(3,1,2,5,4)])
  
  
  shares <- sector_data %>% 
    filter(year %in% c(1990,2000,2010,2018)) %>% 
    group_by(year) %>% 
    mutate(totals=sum(value)) %>% 
    ungroup() %>% 
    group_by(year,chapter) %>% 
    mutate(fractions=(value/totals)*100) %>% 
    ungroup()
  
  shares <- locate_shares(shares,"chapter",4)
  
  line_data <- data.frame(x=c(1990,1990,2000,2000,2010,2010,2018,2018),y=c(0,1.1*max(shares$totals)))
  
  
  p <- sector_data %>%
    ggplot(.,aes(x=year,y=value,fill=chapter_title)) +
    geom_area(colour="#737373") +
    
    geom_text(data=shares,aes(x=year+0.75,y=location,label=paste(round(fractions,0),"%",sep="")),size=3.5,colour="#252525")+
    
    geom_line(inherit.aes = FALSE,data=line_data,aes(x=x,y=y,group=x),alpha=0.3,linetype="dashed") +
    
    #### text with the totals
    geom_text(data=shares %>% filter(chapter_title=="Industry"),aes(x=year,y=1.1*max(shares$totals),label=paste(round(totals,0),  "Gt")),size=3.5,colour="#252525")+
    
    ylab("GHG emissions (GtCO2eq/yr)") +
    
    labs(fill="Sector") +
    #scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
    scale_x_continuous(breaks=c(1990,2000,2010,2018),limits=c(1990,2019)) +
    
    big_trend_theme +
    theme(legend.position="bottom",
          legend.background = element_rect(linetype = 1, size = 0.5, colour = "#525252"),
          legend.margin = margin(l=5,t=5,b=5,r=10,unit="pt"),
          legend.text = element_text(size=8),
          legend.title = element_blank()) +
    ggtitle(paste0("a. ",region," GHG emissions by sector"))
  
  
  return(list("plot"=p,"data"=sector_data))
}

```


# Top emitting sectors

```{r top_subsectors,echo=FALSE,warning=FALSE,results='asis',fig.width=10,fig.height=4.5,fig.path="../Results/Plots/Sectors/",dev=c('png','pdf')}

rates <- edgar_GHG_ar6  %>%
  filter(year>1989) %>%
  filter(year<2019) %>%
  group_by(year,chapter,chapter_title,subsector_title) %>%
  summarise(value=sum(GHG,na.rm=TRUE)/1e9) %>%
  ungroup()

land_totals <- land %>%
  filter(year>1989) %>%
  filter(year<2019) %>%
  mutate(chapter=7) %>%
  mutate(chapter_title="AFOLU") %>%
  group_by(year,chapter,chapter_title,subsector_title) %>%
  summarise(value=sum(value,na.rm=TRUE)/1e9) %>%
  ungroup()

rates <- rbind(rates,land_totals)

time_start=2010

rates <- rates %>%
  filter(year %in% c(time_start,2018)) %>% 
  group_by(subsector_title) %>%
  mutate(rate=(last(value)/first(value))^(1/(last(year)-time_start))-1) %>%
  mutate(abs_growth=last(value)-first(value)) %>%
  filter(year==2018)

### Top emitting sectors in 2018

top_sectors <- rates %>%
  ungroup() %>%
  arrange(desc(value)) %>%
  head(15)

top_sectors <- top_sectors %>%
  mutate(label=paste0(subsector_title," (",chapter_title,")"))


top_sectors <- left_join(top_sectors,edgar_GHG_ar6,by = c("year", "chapter", "chapter_title", "subsector_title"))

top_sectors <- top_sectors %>%
  group_by(label,value,rate,abs_growth,region_ar6_5,region_ar6_5_short) %>%
  summarise_at(vars(CO2,CH4,N2O,Fgas,GHG),sum,na.rm=TRUE) %>%
  mutate_at(vars(CO2,CH4,N2O,Fgas,GHG),list(~./1e9))

top_sectors <- top_sectors %>%
  group_by(label) %>%
  mutate_at(vars(CO2,CH4,N2O,Fgas),sum,na.rm=TRUE)

top_sectors <- top_sectors %>%
  filter(region_ar6_5_short!="SEA")%>%
  filter(region_ar6_5_short!="AIR")

p2 <- top_sectors %>%
  ggplot(.,aes(y=GHG,x=reorder(label,value),fill=region_ar6_5)) +
  geom_bar(stat='identity',colour="#737373") +
  coord_flip() +
  scale_fill_brewer(type = "qual",palette="Set2") +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position = c(0.6,0.3),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ggtitle("Regional composition")

top_sectors <- gather(top_sectors,gas,gasvalue,CO2:Fgas) %>%
  filter(region_ar6_5=="Developed Countries")


p1 <- top_sectors %>%
  ggplot(.,aes(y=gasvalue,x=reorder(label,value),fill=reorder(gas,gasvalue))) +
  geom_bar(stat='identity',colour="#737373") +
  geom_text(data=top_sectors %>% filter(gas=="CO2"),aes(y=value+1.5,x=reorder(label,value),label=paste0(ifelse(rate>0,"+","-"),round(abs(rate)*100,1),"%"))) +
  coord_flip() +
  ylim(0,16) +
  scale_fill_brewer(type = "qual",palette="Set2") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.75,0.3),
        legend.title = element_blank(),
        legend.background = element_blank()) +
  ggtitle("Top emitting sectors in 2018, growth since 2010") +
  ylab("GHG emissions (Gt CO2eq)")

ggarrange(p1,p2,align="h",widths=c(0.7,0.3),nrow=1,ncol=2)
p1



```















### Regional per capita trends (Figure 6)

```{r percapita_trends, echo=FALSE,warning=FALSE,fig.width=8,fig.height=4,fig.path="Results/Plots/",dev=c('png','pdf')}

######
# load('../Data/edgar.RData')
# load('../Data/basic.RData')
# 
# edgar_GHG <- left_join(edgar_GHG,basic %>% select(ISO,pop_UN,Year,gdp_real_WB),by=c("ISO"="ISO","year"="Year"))
# 
# ######
# 
# region <- edgar_GHG %>% 
#   filter(year>1969 & year<2018) %>% 
#   select(year,region_wb,country,GHG,gdp_real_WB,pop_UN) %>% 
#   group_by(year,country,region_wb) %>% 
#   summarise(GHG=sum(GHG,na.rm=T),pop_UN=first(pop_UN),gdp_real_WB=first(gdp_real_WB)) %>% 
#   group_by(year,region_wb) %>% 
#   summarise(ghg_total = sum(GHG,na.rm=T)*1000,pop=sum(pop_UN,na.rm=T),gdp=sum(gdp_real_WB,na.rm=T)) %>% 
#   mutate(ghg_pc = ghg_total/pop,ghg_gdp = ghg_total/gdp)
#   
# ## per capita ghg emissions (line chart)
# p3 <- region %>% 
#   filter(!is.na(region_wb)) %>% 
#   ggplot(.,aes(x=year,y=ghg_pc,colour=region_wb)) +
#   ylab('GHG Emissions per capita (tCO2eq/capita/yr)') +
#   geom_path(size=1) +
#   theme_bw() +
#   theme(legend.position = c(0.12,0.8),
#         legend.title=element_blank(),
#         legend.background = element_blank(),
#         axis.title.x = element_blank())
# 
# ## ghg emissions per gdp (line chart)
# p4 <- region %>% 
#   filter(!is.na(region_wb)) %>% 
#   ggplot(.,aes(x=year,y=ghg_gdp,colour=region_wb)) +
#   ylab('GHG Emissions per $GDP (tCO2eq/$/yr)') +
#   theme_bw() +
#   geom_path(size=1) +
#   theme(legend.title = element_blank(), legend.position = "none",
#         axis.title.x = element_blank())
# 
# 
# #ggarrange(p1, p2, p3, p4, nrow=2,ncol=2)
# ggarrange(p3,p4,nrow=1,ncol=2)
# 
# xlsx::write.xlsx(as.data.frame(region),file=paste("Results/IPCC_plot_data_",Sys.Date(),".xlsx",sep=""),sheetName = "per_capita_trends",append=T,row.names=FALSE)


```






###### waterfall AR5 GWPs

waterfall <- plot_data %>%
  filter(year==2018) %>% 
  group_by(gas) %>%
  summarise(value=sum(value)) %>% 
  arrange(desc(gas))

waterfall$end <- cumsum(waterfall$value)
waterfall$start <- c(0, head(waterfall$end, -1))
waterfall <- cbind(waterfall,id=c(5,4,3,2,1))

p3 <- waterfall %>%
  ggplot(.,aes(fill=gas)) +
  geom_rect(aes(x = gas,
                xmin = id - 0.4,
                xmax = id + 0.4,
                ymin = start,
                ymax = end),colour="#737373") +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
  theme_bw() +
  ylim(0,62) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank())

###### waterfall AR6 GWPs

second_waterfall <- plot_data %>%
  filter(year==2018) %>% 
  group_by(gas) %>%
  summarise(value=sum(value))%>% 
  arrange(desc(gas))

second_waterfall <- left_join(second_waterfall,gwps,by=c("gas"="gas"))

second_waterfall$gas <- as.factor(second_waterfall$gas)
second_waterfall$gas <-  factor(second_waterfall$gas,levels(second_waterfall$gas)[c(4,5,1,3,2)])


second_waterfall <- second_waterfall %>% 
  mutate(value=ifelse(gas=="CH4",(value/gwp_ar5)*gwp_ar6,value)) %>% 
  mutate(value=ifelse(gas=="N2O",(value/gwp_ar5)*gwp_ar6,value)) %>% 
  select(gas,value)

second_waterfall$end <- cumsum(second_waterfall$value)
second_waterfall$start <- c(0, head(second_waterfall$end, -1))
second_waterfall <- cbind(second_waterfall,id=c(5,4,3,2,1))

p4 <- second_waterfall %>%
  ggplot(.,aes(fill=gas)) +
  geom_rect(aes(x = gas,
                xmin = id - 0.4,
                xmax = id + 0.4,
                ymin = start,
                ymax = end),colour="#737373") +
  scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
  theme_bw() +
  ylim(0,62) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank())


###### waterfall AR2 GWPs

third_waterfall <- plot_data %>%
  filter(year==2018) %>% 
  group_by(gas) %>%
  summarise(value=sum(value))%>% 
  arrange(desc(gas))

third_waterfall <- left_join(third_waterfall,gwps,by=c("gas"="gas"))

third_waterfall$gas <- as.factor(third_waterfall$gas)
third_waterfall$gas <-  factor(third_waterfall$gas,levels(third_waterfall$gas)[c(4,5,1,3,2)])


third_waterfall <- third_waterfall %>% 
  mutate(value=ifelse(gas=="CH4",(value/gwp_ar5)*gwp_ar2,value)) %>% 
  mutate(value=ifelse(gas=="N2O",(value/gwp_ar5)*gwp_ar2,value)) %>% 
  select(gas,value)

third_waterfall$end <- cumsum(third_waterfall$value)
third_waterfall$start <- c(0, head(third_waterfall$end, -1))
third_waterfall <- cbind(third_waterfall,id=c(5,4,3,2,1))

p5 <- waterfall %>%
  ggplot(.,aes(fill=gas)) +
  geom_rect(aes(x = gas,
                xmin = id - 0.4,
                xmax = id + 0.4,
                ymin = start,
                ymax = end),colour="#737373") +
  #scale_y_continuous(breaks=c(0,10,20,30,40,50,60)) +
  theme_bw() +
  ylim(0,62) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank())









## this was in category_mapping, not sure what for

my_sheet <- read.xlsx('Data/IPCC_master_categories.xlsx',sheetName='full_list')

ar5_map <- read.xlsx('Data/IPCC_AR5_sector_mapping.xlsx',sheetName='EmissionMap',startRow = 3,endRow = 132) %>%
  select(code=IPCC.cat.,IPCC_AR5_sector=Sector.1,IPCC_AR5_chapter=Chapter.1,IPCC_AR5_description=IPCC_description)

z <- full_join(my_sheet,thing,by=("code"="code")) %>%
  select(code,IPCC_1996_description,EDGAR_description,IPCC_AR5_description,IPCC_AR5_sector,IPCC_AR5_chapter)
  arrange(code)

write.xlsx(z,'Results/IPCC_master_categories.xlsx',sheetName='code_comparisons',row.names = FALSE,append=TRUE)



####



### Regional trends & rates of change by IPCC sector


```{r edgar_plots_rates,echo=FALSE,warning=FALSE,fig.width=8,fig.height=10,fig.path="../Results/Plots/",dev=c('png','pdf')}


########### rates of change: sectors 7-11
load('../Data/edgar.RData')
c=1
data = data.frame()
plot_list = list()
time_start = 2010
category = 'chapter'
gas = 'GHG'

for (i in 6:7) {
  
  p <- edgar_emissions_plot(time_start,category,i,gas,'rate')
  plot_list[[c]] <- p$plot
  
  #if (c==1) {
  #  data <- p$data %>% 
  #    mutate(run=paste(category,i,gas,time_start,sep="_"))
  #} else {
  #  data <- rbind(data,p$data %>% mutate(run=paste(category,i,gas,time_start,sep="_")))
  #}
  
  c=c+1
  
}
# for (i in 9:11) {
#   
#   p <- edgar_emissions_plot(time_start,category,i,gas,'rate')
#   plot_list[[c]] <- p$plot
#   c=c+1
#   
# }

plot_list

#write.xlsx(data %>% select(-Year),file=paste("../Results/IPCC_plot_data_",Sys.Date(),".xlsx",sep=""),sheetName=paste(category,gas,time_start,sep="_"),append=TRUE)


```



```{r edgar_plots_abs,echo=FALSE,warning=FALSE,fig.width=8,fig.height=10,fig.path="../Results/Plots/",dev=c('png','pdf')}

########### absolute growth: sectors 7-11
# c=1
# data = data.frame()
# plot_list = list()
# time_start = 2010
# category = 'chapter'
# gas = 'GHG'
# 
# for (i in 7:11) {
#   
#   p <- edgar_emissions_plot(time_start,category,i,gas,'abs_growth')
#   
#   plot_list[[c]] <- p$plot
#   
#   if (c==1) {
#     data <- p$data %>% 
#       mutate(run=paste(category,i,gas,time_start,sep="_"))
#   } else {
#     data <- rbind(data,p$data %>% mutate(run=paste(category,i,gas,time_start,sep="_")))
#   }
#   
#   c=c+1
#   
# }
# plot_list
# 


```









# old code






### Growth rates


```{r growth_rates, echo=FALSE,warning=FALSE}

rates <- basic %>% 
  filter(Country!="World") %>% 
  filter(Year>1979 & Year<2018) %>% 
  mutate(co2_terr_pc = co2_terr_GCB/pop_UN) %>% 
  mutate(co2_cons_pc = co2_cons_GCB/pop_UN) %>% 
  mutate(gdp_ppp_pc = gdp_ppp_WB/pop_UN)# %>% 
#select(Country,UN6,Year,co2_terr_pc,pop_UN,co2_terr_GCB,health_lex_WB)

subset <- rates %>% 
  group_by(Country) %>% 
  filter(Year>1999 & Year<2006) %>% 
  summarise(average=mean(co2_terr_GCB))

rates <- left_join(rates,subset %>% select(Country,average),by=c("Country"="Country"))

rates <- rates %>% 
  group_by(Country,Year) %>% 
  mutate(rate = 1-(average/co2_terr_GCB))

# filter by years to assess here (last 10?)
models <- rates %>% 
  filter(Year>2004) %>% 
  na.omit() %>% 
  group_by(Country) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(rate ~ Year, data = .x)),
         tidy_model = map(model, broom::tidy)) %>% 
  unnest(tidy_model) %>% 
  mutate(intercept = first(estimate))

rates <- left_join(rates,models %>% filter(term=='Year') %>% select(-term),by=c("Country"="Country"))

```


``` {r rates_summary, echo=FALSE, warning=FALSE,fig.width=8}

# blarg <- rates %>% 
#   na.omit() %>% 
#   group_by(Country,Year) %>% 
#   filter(max(pop_UN)>1e6) %>% 
#   filter(std.error<0.01) %>% 
#   arrange(estimate)


# rank countries by rate of change
#blarg$Country <- 

# blarg %>% 
#   filter(Year==2014) %>% 
#   ggplot(aes(y=rate,x=Country)) +
#   geom_bar(stat='identity') +
#   coord_flip()



```

<strong>Fast decline in total emissions, relative to 2000-2005</strong>  
  
  ``` {r decline, echo=FALSE,warning=FALSE,fig.width=8,fig.path="Plots/",dev=c('png')}

## choose subset (fast decline)
blarg<- rates %>%
  na.omit() %>% 
  group_by(Country,Year) %>% 
  filter(max(pop_UN)>1e6) %>% 
  filter(std.error<0.01) %>% 
  arrange(estimate) %>%
  group_by(Year) %>% 
  do(head(., n = 20))

# plot 
blarg %>% 
  ggplot(aes(x=Year,y=rate))+
  geom_point(colour='grey') +
  geom_line(colour='grey') +
  geom_smooth(data=subset(blarg, Year>2004),method = "lm") +
  facet_wrap(. ~ Country,ncol=5) +
  ylim(-0.5,0.5)

```

<strong>Fast growth in total emissions, relative to 2000-2005</strong>  
  
  ``` {r increase, echo=FALSE,warning=FALSE,fig.width=8,fig.path="Plots/",dev=c('png')}
## choose subset (fast increase)
blarg<- rates %>%
  na.omit() %>% 
  group_by(Country,Year) %>% 
  filter(max(pop_UN)>1e6) %>% 
  filter(std.error<0.01) %>% 
  arrange(desc(estimate)) %>%
  group_by(Year) %>% 
  do(head(., n = 20))

# plot 
blarg %>% 
  ggplot(aes(x=Year,y=rate))+
  geom_point(colour='grey') +
  geom_line(colour='grey') +
  geom_smooth(data=subset(blarg, Year>2004),method = "lm") +
  facet_wrap(. ~ Country,ncol=5) +
  ylim(-0.5,0.5)

```

<strong>Fast increase in total emissions, relative to 2000-2005, with high well-being (70yrs life exp.)</strong>  
  
  ``` {r increase_WB, echo=FALSE,warning=FALSE,fig.width=8,fig.path="Plots/",dev=c('png')}

## choose subset (fast increase)
blarg<- rates %>%
  na.omit() %>% 
  group_by(Country) %>% 
  filter(max(pop_UN)>1e6) %>% 
  filter(max(health_lex_WB)>75) %>%
  filter(std.error<0.01) %>% 
  group_by(Country,Year)%>% 
  arrange(desc(estimate)) %>%
  group_by(Year) %>% 
  do(head(., n = 20))

# plot 
blarg %>% 
  ggplot(aes(x=Year,y=rate))+
  geom_point(colour='grey') +
  geom_line(colour='grey') +
  geom_smooth(data=subset(blarg, Year>2004),method = "lm") +
  facet_wrap(. ~ Country,ncol=5) +
  ylim(-0.5,0.5) +
  ylab('Change in CO2, relative to 2000-2005 (%)') +
  theme(axis.title.x = element_blank()) 


```

<strong>Fast increase in total emissions, relative to 2000-2005, with high income (>10,000)</strong>  
  
  ``` {r increase_income, echo=FALSE,warning=FALSE,fig.width=8,fig.path="Plots/",dev=c('png')}

## choose subset (fast increase)
blarg<- rates %>%
  na.omit() %>% 
  group_by(Country) %>% 
  filter(max(pop_UN)>1e6) %>% 
  filter(max(gdp_ppp_pc)>15000) %>%
  filter(std.error<0.01) %>% 
  group_by(Country,Year)%>% 
  arrange(desc(estimate)) %>%
  group_by(Year) %>% 
  do(head(., n = 10))

# plot 
blarg %>% 
  ggplot(aes(x=Year,y=rate))+
  geom_point(colour='grey') +
  geom_line(colour='grey') +
  geom_smooth(data=subset(blarg, Year>2004),method = "lm") +
  facet_wrap(. ~ Country,ncol=5) +
  ylim(-0.5,0.5) +
  ylab('Change in CO2, relative to 2000-2005 (%)') +
  theme(axis.title.x = element_blank()) 


```


```{r regional_trends, echo=FALSE,warning=FALSE,fig.width=8,fig.height=10,fig.path="Plots/",dev=c('png','pdf')}

selection = c("co2_terr_GCB","pop_UN")
time_start = 2010

##############  calculate growth rates in given time period ############## 

rates <- basic %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

####### CHECK DATA HERE FOR ZEROS AND NANS ####### 

## remove countries below absolute emissions threshold

rates <- rates %>% 
  group_by(Country) %>% 
  mutate(rate=(last(co2_terr_GCB)/first(co2_terr_GCB))^(1/(last(Year)-time_start+1))-1) %>% 
  filter(Year==2017) %>% 
  na.omit() %>% 
  arrange(desc(co2_terr_GCB))

# what is 80% of the emissions?
threshold <- basic %>% 
  filter(Country=="World") %>% 
  filter(Year==2017) %>% 
  summarise_at(selection,funs(.*0.80))

threshold <- as.vector(threshold[,grep(selection[1],colnames(threshold))])
rates$cutoff <- 0
z <- 0
for (i in 1:length(rates$Country)){
  z <- z + rates$co2_terr_GCB[i]
  if (z > threshold) {
    break
  }
  rates$cutoff[i] <- 1
}

country_list <- rates %>% select(Country,cutoff)

#### calculate growth rates for countries below threshold, grouped by region ####

blarg <- basic %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

blarg <- left_join(blarg,country_list,by=c("Country"="Country")) %>% 
  filter(cutoff==0) %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(WB.income,Year) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

blarg <- blarg %>% 
  group_by(WB.income) %>% 
  mutate(rate=(last(co2_terr_GCB)/first(co2_terr_GCB))^(1/(last(Year)-time_start+1))-1) %>% 
  filter(Year==2017) %>% 
  na.omit() %>% 
  mutate(Country=paste(WB.income,"(rest)"))


### join and plot
rates <- rbind(rates %>% filter(cutoff==1) %>% select(-cutoff),blarg)

p2 <- rates %>% 
  ggplot(.,aes(x = reorder(Country,rate), y = rate*100, fill=WB.income)) +
  geom_bar(stat='identity') + 
  ylab(paste("Rate of change in emissions, ",time_start,"- 2017")) +
  coord_flip() +
  #facet_grid(WB.income~.,scales="free_y") +
  theme(legend.position="none",
        axis.title.y = element_blank()) 


############## plot absolute values, trend over time ############## 

regions <- left_join(basic,country_list,by=c("Country"="Country")) %>%
  filter(!is.na(WB.income)) %>% 
  filter(Country!="World") %>% 
  filter(Year>1980 & Year<2018) %>% 
  filter(cutoff==0) %>% 
  group_by(WB.income,Year) %>% 
  summarise_at(selection,sum,na.rm=TRUE) %>% 
  mutate(Country=paste(WB.income,"(rest)")) %>% 
  ungroup()

countries <- left_join(basic,country_list,by=c("Country"="Country")) %>%
  filter(!is.na(WB.income)) %>% 
  filter(Country!="World") %>% 
  filter(Year>1980 & Year<2018) %>% 
  filter(cutoff==1) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE) %>% 
  ungroup()

plot_data <- rbind(countries,regions)

rect <- data.frame(xmin=time_start, xmax=2017, ymin=-Inf, ymax=Inf)

# plot_data %>% 
#   #filter(WB.income!="High income") %>% 
#   ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
#   geom_area(color="black")

#plot_data %>% 
# ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
#geom_bar(stat='identity')

p1 <- regions %>%
  ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
  geom_area() +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="grey20",
            alpha=0.35,
            inherit.aes = FALSE) +
  theme(legend.position='bottom',
        legend.title=element_blank())


ggarrange(p1,p2,ncol=1,nrow=2)

```



# fossil_data <- fft %>% 
#   select(-Per.Capita,-Total) %>% 
#   group_by(Year) %>% 
#   mutate_at(vars(-group_cols()),funs(.*3.664)) %>% 
#   mutate_at(vars(-group_cols()),funs(./1000))

# budget_data <- gcb %>% 
#   select(-ocean.sink,-land.sink,-atmospheric.growth,-budget.imbalance) %>% 
#   group_by(Year) %>% 
#   mutate_at(vars(-group_cols()),funs(.*3.664))

# plot_data <- left_join(budget_data,fossil_data,by=c("Year"="Year")) %>% 
#   select(-fossil.fuel.and.industry)
# 
# plot_data <- gather(plot_data,key,value,-Year)
# plot_data$key <- gsub("land.use.change.emissions","Land use change",plot_data$key)
# plot_data$key <- as.factor(plot_data$key)
# plot_data$key <- factor(plot_data$key,levels(plot_data$key)[c(3,1,4,2,6,5)])
# 
# 
# p1 <- plot_data %>% 
#   ggplot(.,aes(x=Year,y=value,fill=key)) +
#   geom_area() +
#   ylab("Fossil fuel, cement & land use change emissions (GtCO2/yr)") +
#   theme(legend.position = c(0.15,0.75),
#         legend.title = element_blank(),
#         axis.title.x = element_blank())









``` {blarg}

selection = c("co2_terr_GCB","pop_UN")
time_start = 2010

##############  calculate growth rates in given time period ############## 

rates <- basic %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

####### CHECK DATA HERE FOR ZEROS AND NANS ####### 

## remove countries below absolute emissions threshold

rates <- rates %>% 
  group_by(Country) %>% 
  mutate(rate=(last(co2_terr_GCB)/first(co2_terr_GCB))^(1/(last(Year)-time_start+1))-1) %>% 
  filter(Year==2017) %>% 
  na.omit() %>% 
  arrange(desc(co2_terr_GCB))

# what is 80% of the emissions?
threshold <- basic %>% 
  filter(Country=="World") %>% 
  filter(Year==2017) %>% 
  summarise_at(selection,funs(.*0.80))

threshold <- as.vector(threshold[,grep(selection[1],colnames(threshold))])
rates$cutoff <- 0
z <- 0
for (i in 1:length(rates$Country)){
  z <- z + rates$co2_terr_GCB[i]
  if (z > threshold) {
    break
  }
  rates$cutoff[i] <- 1
}

country_list <- rates %>% select(Country,cutoff)

#### calculate growth rates for countries below threshold, grouped by region ####

blarg <- basic %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

blarg <- left_join(blarg,country_list,by=c("Country"="Country")) %>% 
  filter(cutoff==0) %>% 
  filter(!is.na(WB.income)) %>% 
  filter(Year>=time_start & Year<2018) %>% 
  group_by(WB.income,Year) %>% 
  summarise_at(selection,sum,na.rm=TRUE)

blarg <- blarg %>% 
  group_by(WB.income) %>% 
  mutate(rate=(last(co2_terr_GCB)/first(co2_terr_GCB))^(1/(last(Year)-time_start+1))-1) %>% 
  filter(Year==2017) %>% 
  na.omit() %>% 
  mutate(Country=paste(WB.income,"(rest)"))


### join and plot
rates <- rbind(rates %>% filter(cutoff==1) %>% select(-cutoff),blarg)

p2 <- rates %>% 
  ggplot(.,aes(x = reorder(Country,rate), y = rate*100, fill=WB.income)) +
  geom_bar(stat='identity') + 
  ylab(paste("Rate of change in emissions, ",time_start,"- 2017")) +
  coord_flip() +
  #facet_grid(WB.income~.,scales="free_y") +
  theme(legend.position="none",
        axis.title.y = element_blank()) 


############## plot absolute values, trend over time ############## 

regions <- left_join(basic,country_list,by=c("Country"="Country")) %>%
  filter(!is.na(WB.income)) %>% 
  filter(Country!="World") %>% 
  filter(Year>1980 & Year<2018) %>% 
  filter(cutoff==0) %>% 
  group_by(WB.income,Year) %>% 
  summarise_at(selection,sum,na.rm=TRUE) %>% 
  mutate(Country=paste(WB.income,"(rest)")) %>% 
  ungroup()

countries <- left_join(basic,country_list,by=c("Country"="Country")) %>%
  filter(!is.na(WB.income)) %>% 
  filter(Country!="World") %>% 
  filter(Year>1980 & Year<2018) %>% 
  filter(cutoff==1) %>% 
  group_by(Country,Year,WB.income) %>% 
  summarise_at(selection,sum,na.rm=TRUE) %>% 
  ungroup()

plot_data <- rbind(countries,regions)

rect <- data.frame(xmin=time_start, xmax=2017, ymin=-Inf, ymax=Inf)

# plot_data %>% 
#   #filter(WB.income!="High income") %>% 
#   ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
#   geom_area(color="black")

#plot_data %>% 
# ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
#geom_bar(stat='identity')

p1 <- regions %>%
  ggplot(.,aes(x=Year,y=co2_terr_GCB,fill=WB.income)) +
  geom_area() +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill="grey20",
            alpha=0.35,
            inherit.aes = FALSE) +
  theme(legend.position='bottom',
        legend.title=element_blank())


ggarrange(p1,p2,ncol=1,nrow=2)


```







```{r summary_text, include = FALSE}

summary_text <- function(ch,fraction,trend) {
  
  total <- trend %>% ungroup() %>% filter(Year==2017) %>% summarise(sum=sum(value))
  growth <- trend %>% 
    ungroup() %>% 
    group_by(Year) %>% 
    summarise(value=sum(value))
  compound <- ((tail(growth$value,1)/growth$value[1])^(1/length(growth$Year))-1)*100
  growth <- ((tail(growth$value,1)-growth$value[1])/growth$value[1])*100
  
  text <- paste0(sectors$value[sectors$key==ch]," emissions were ",round(total$sum,2), "Gt CO2 in 2017, ",fraction$value,"% of total GHG emissions from all sectors. ",sectors$value[sectors$key==ch]," emissions have grown by ",round(growth,2),"% between 1970 and 2017.")
  
  return(text)
}


```



# 1. All sectors

```{r total_sector_trends,echo=FALSE,warning=FALSE,fig.width=8,fig.height=10,fig.path="../Results/Plots/",dev=c('png','pdf')}
# 
# load('../Data/edgar_data_gwp_ar5.RData')
# 
# ########### Trend figure ###########
# 
# totals <- edgar_GHG_ar5 %>% 
#   filter(year>1989) %>% 
#   filter(year<2018) %>% 
#   group_by(ISO,country,year,region_ar6_5) %>% 
#   summarise_at(c("CO2","CH4","N2O","Fgas","GHG"),sum,na.rm=TRUE) %>% 
#   mutate(chapter=0,sector_code="Total",description="Total",category_1="Total",category_2="Total",category_3="Total")
# 
#   
# trend <- trend_figure(2010,"sector_code","Total","GHG",totals,big_trend_theme)
# p1 <- trend$plot
# p1 <- p1 + ggtitle("A. Trend in total emissions and income classification ")
# d1 <- trend$data
# 
# ########### Decomposition figure ###########
# 
# decomp <- decomp_figure_countries(2010,'GHG',edgar_GHG_ar5)
# p2 <- ggarrange(decomp$rate_plot,decomp$abs_plot,ncol=2,nrow=1)
# p2 <- annotate_figure(p2,
#   top = text_grob("B. Decomposition of relative and absolute contribution by country",size = 12,hjust=0.75))
# 
# plot <- ggarrange(p1,p2,ncol=1,nrow=2,heights=c(0.4,0.6))
# plot
# 
# 
# #write.xlsx(p$data %>% select(-Year),file=paste("../Results/IPCC_plot_data_",Sys.Date(),".xlsx",sep=""),sheetName="Total_GHGs_2010",append=TRUE)

```
