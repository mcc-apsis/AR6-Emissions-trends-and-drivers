# old code


### Regional trends
<strong>i.e. Figure 5.2 from IPCC AR5 (booorring)</strong>  
  
  ```{r regional_trends_territorial, echo=FALSE,warning=FALSE,fig.width=10,fig.height=5}

region <- basic %>%
  filter(Country!="World") %>% 
  filter(Year>1969 & Year<2018) %>% 
  filter(!is.na(WB.income)) %>% 
  group_by(WB.income,Year) %>% 
  summarise(co2_territorial_total = sum(co2_terr_GCB,na.rm=TRUE),pop_total=sum(pop_UN)) %>% 
  mutate(co2_territorial_pc = co2_territorial_total/pop_total)

## cumulative emissions total (horizontal bar chart)
p1 <- region %>% 
  group_by(WB.income) %>% 
  summarise(cumulative_co2 = sum(co2_territorial_total)) %>% 
  ggplot(.,aes(y=cumulative_co2,x=WB.income,fill=WB.income)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme(legend.title = element_blank(), legend.position = "none") +
  theme(axis.title.y = element_blank()) 

## total territorial emissions (stacked area)
p2 <- region %>% ggplot(.,aes(x=Year,y=co2_territorial_total,fill=WB.income)) +
  geom_area() +
  theme(legend.title = element_blank(),legend.position = "none")

## per capita territorial emissions (line chart)
p3 <- region %>% ggplot(.,aes(x=Year,y=co2_territorial_pc,colour=WB.income)) +
  geom_path(size=1) +
  theme(legend.title = element_blank(), legend.position = "none")

ggarrange(p1, p2, p3, ncol=3)

```



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



