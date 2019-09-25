######## decomposition figure (BY COUNTRIES)

# load('Data/edgar.RData')
# sector=6
# gas="GHG"
# time_start=2010
# category="chapter"


trend_figure_sectors <- function(time_start,category,sector,gas,edgar_GHG) {
  
  data <- edgar_GHG %>%
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>%
    group_by(sector_code,description,Year) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    rename_at(gas,funs(sprintf('value')))
  
  # what is x% of the emissions?
  blarg <- data %>%
    filter(Year==2017)
  threshold <- sum(blarg$value,na.rm=T)*0.9
  
  ## remove countries below absolute emissions threshold
  
  rates <- data %>% 
    group_by(sector_code) %>% 
    filter(Year==2017) %>% 
    na.omit() %>% 
    arrange(desc(value))
  
  
  rates$cutoff <- 0
  z <- 0
  for (i in 1:length(rates$sector_code)){
    z <- z + rates$value[i]
    if (z > threshold) {
      break
    }
    rates$cutoff[i] <- 1
  }
  
  sector_list <- rates %>% select(sector_code,cutoff)
  
  
  #### sum up rest of the sectors
  
  blarg <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>% 
    group_by(sector_code,description,Year) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('value')))
  
  blarg <- left_join(blarg,sector_list,by=c("sector_code"="sector_code")) %>% 
    filter(cutoff==0) %>%
    group_by(Year) %>% 
    summarise(value=sum(value)) %>% 
    mutate(sector_code="Rest") %>% 
    mutate(description="Rest")
  
  data <- left_join(data,sector_list,by=c("sector_code"="sector_code")) %>% 
    filter(cutoff==1) %>% 
    select(-cutoff) %>% 
    ungroup()
  
  ### join
  data <- rbind(data,blarg) %>% 
    mutate(value=value/1e6) %>% 
    mutate(label=paste(sector_code,"-",description))
    
  
  # shares <- data %>% 
  #   filter(Year %in% c(1970,1980,1990,2000,2010,2017)) %>% 
  #   group_by(Year) %>% 
  #   mutate(totals=sum(value)) %>% 
  #   ungroup() %>% 
  #   group_by(Year,sector_code) %>% 
  #   mutate(fractions=(value/totals)*100)
  # 
  # shares <- shares %>%
  #   arrange(Year)
  # 
  # for (i in seq(1,24,4)) {
  #   
  #   shares$location[i] = (shares$value[i]/2) + shares$value[i+1] + shares$value[i+2] + shares$value[i+3]
  #   shares$location[i+1] = (shares$value[i+1]/2) + shares$value[i+2] + shares$value[i+3]
  #   shares$location[i+2] = (shares$value[i+2]/2) + shares$value[i+3]
  #   shares$location[i+3] = (shares$value[i+3]/2)
  #   
  # }
  
  p <- data %>%
    ggplot(.,aes(x=Year,y=value,fill=label)) +
    geom_area(color='#737373') +
    #geom_text(data=shares,aes(x=Year,y=location,label=paste(round(fractions,0),"%",sep="")))+
    geom_vline(xintercept=c(1970,1980,1990,2000,2010,2017),alpha=0.3,linetype="dashed") +
    theme_bw() +
    ggtitle("A. Trends by subsector") +
    theme(legend.justification=c(0,1), 
          legend.position=c(0.05, 0.97),
          legend.title=element_blank(),
          legend.background = element_blank(),
          legend.spacing.x = unit(0.2,'cm'),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 12)) +
    ylab(bquote(.(gas) ~" Emissions (Gt" ~CO[2]* "eq)"))
  
  return(list("plot"=p,"data"=data))
  
}





decomp_figure_sectors <- function(time_start,category,sector,gas,edgar_GHG) {
  
  ##############  calculate growth rates in given time period ############## 
  
  rates <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(sector_code,description,Year) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('value')))
  
  ####### CHECK DATA HERE FOR ZEROS AND NANS ####### 
  
  # what is 75% of the emissions?
  blarg <- rates %>%
    filter(Year==2017)
  threshold <- sum(blarg$value,na.rm=T)*0.9
  
  ## remove countries below absolute emissions threshold
  
  rates <- rates %>% 
    group_by(sector_code) %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(Year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(Year==2017) %>% 
    na.omit() %>% 
    arrange(desc(value))
  
  
  rates$cutoff <- 0
  z <- 0
  for (i in 1:length(rates$sector_code)){
    z <- z + rates$value[i]
    if (z > threshold) {
      break
    }
    rates$cutoff[i] <- 1
  }
  
  sector_list <- rates %>% select(sector_code,cutoff)
  
  #### calculate growth rates for the rest of the sectors
  
  blarg <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(WB.income)) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(sector_code,description,Year) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('value')))
  
  blarg <- left_join(blarg,sector_list,by=c("sector_code"="sector_code")) %>% 
    filter(cutoff==0) %>% 
    filter(Year>=time_start & Year<2018) %>% 
    group_by(Year) %>% 
    summarise_at('value',sum,na.rm=TRUE)
  
  blarg <- blarg %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(Year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(Year==2017) %>% 
    na.omit() %>% 
    mutate(sector_code="Rest") %>% 
    mutate(description="Rest")
  
  ### join
  rates <- rbind(rates %>% filter(cutoff==1) %>% select(-cutoff) %>% ungroup(),blarg) %>% 
    mutate(label=paste(sector_code,"-",description))
  rates <- gather(rates %>% mutate(rate=rate*100),var,value,rate:abs_growth)
  
  #rate
  p1 <- rates %>% 
    filter(var=="rate") %>% 
    ggplot(.,aes(x = reorder(label,value),y = value,fill=label)) +
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
    ggplot(.,aes(x = reorder(label,value),y = value/1e6,fill=label)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank()) 
  
  return(list("rate_plot"=p1,"abs_plot"=p2,"data"=rates))
}
