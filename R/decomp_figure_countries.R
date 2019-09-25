######## decomposition figure (BY COUNTRIES)

decomp_figure_countries <- function(time_start,category,sector,gas,edgar_GHG) {
  
  ##############  calculate growth rates in given time period ############## 
  
  rates <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(Country,Year,WB.income) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('value')))
  
  ####### CHECK DATA HERE FOR ZEROS AND NANS ####### 
  
  
  # what is 75% of the emissions?
  blarg <- rates %>%
    filter(Year==2017)
  threshold <- sum(blarg$value,na.rm=T)*0.75
  
  ## remove countries below absolute emissions threshold
  
  rates <- rates %>% 
    group_by(Country) %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(Year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(Year==2017) %>% 
    na.omit() %>% 
    arrange(desc(value))
  
  
  rates$cutoff <- 0
  z <- 0
  for (i in 1:length(rates$Country)){
    z <- z + rates$value[i]
    if (z > threshold) {
      break
    }
    rates$cutoff[i] <- 1
  }
  
  country_list <- rates %>% select(Country,cutoff)
  
  #### calculate growth rates for countries below threshold, grouped by region ####
  
  blarg <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(Country,Year,WB.income) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('value')))
  
  blarg <- left_join(blarg,country_list,by=c("Country"="Country")) %>% 
    filter(cutoff==0) %>% 
    filter(!is.na(WB.income)) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(WB.income,Year) %>% 
    summarise_at('value',sum,na.rm=TRUE)
  
  blarg <- blarg %>% 
    group_by(WB.income) %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(Year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(Year==2017) %>% 
    na.omit() %>% 
    mutate(Country=paste(WB.income,"(rest)"))
  
  
  ### join and plot
  rates <- rbind(rates %>% filter(cutoff==1) %>% select(-cutoff),blarg)
  rates <- gather(rates %>% mutate(rate=rate*100),var,value,rate:abs_growth)
  
  ### shortened names
  
  rates <- rates %>%
    ungroup() %>% 
    mutate(Country = ifelse(Country=="High income (rest)","HI (rest)",Country)) %>% 
    mutate(Country = ifelse(Country=="Low income (rest)","LI (rest)",Country)) %>% 
    mutate(Country = ifelse(Country=="Lower middle income (rest)","LMI (rest)",Country)) %>% 
    mutate(Country = ifelse(Country=="Upper middle income (rest)","UMI (rest)",Country))
  
  #rate
  p1 <- rates %>% 
    filter(var=="rate") %>% 
    ggplot(.,aes(x = reorder(Country,value),y = value, fill=WB.income)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop("Rate of change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    #ylab(paste("Rate of change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          plot.title = element_text(size = 12)) 
  
  
  #absolute
  p2 <- rates %>% 
    filter(var=="abs_growth") %>% 
    ggplot(.,aes(x = reorder(Country,value),y = value/1e6, fill=WB.income)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank()) 
  
  return(list("rate_plot"=p1,"abs_plot"=p2,"data"=rates))
}
