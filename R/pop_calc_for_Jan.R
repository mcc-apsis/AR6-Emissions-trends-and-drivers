library(tidyverse)
load('Data/basic.RData')
load('Data/ipcc_regions.RData')

pop <- basic %>% 
  select(ISO,Year,pop_UN)

pop <- left_join(pop,ipcc_regions %>% select(ISO,region_ar6_dev))

wld <- pop %>% filter(Year==2018) %>% filter(ISO=="WLD")

pop <- pop %>% 
  filter(Year==2018) %>% 
  filter(ISO!="WLD")

pop <- pop %>% 
  filter(!is.na(region_ar6_dev)) %>% 
  group_by(region_ar6_dev) %>% 
  summarise(pop=sum(pop_UN)) %>% 
  mutate(pop_fraction=(pop/wld$pop_UN)*100)

