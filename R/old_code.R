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
