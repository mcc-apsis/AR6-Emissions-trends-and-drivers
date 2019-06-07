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