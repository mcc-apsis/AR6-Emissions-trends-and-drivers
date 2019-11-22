######## decomposition figure (BY COUNTRIES)

# time_start=2010
# category='sector_code'
# sector='Total'
# gas='GHG'
# edgar_GHG <- totals

decomp_figure_countries <- function(time_start,category,sector,gas,edgar_GHG,basic) {
  
  # set palette
  ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set2") + scale_fill_brewer(palette="Set2")
  
  
  ##############  calculate growth rates in given time period ############## 
  
  rates <- edgar_GHG %>% 
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(region_ar6_5_short)) %>% 
    filter(year>=time_start & year<2018) %>% 
    group_by(country,ISO,year,region_ar6_5_short) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('emissions_total')))
  
  ####### CHECK DATA HERE FOR ZEROS AND NANS ####### 
  
  ### join pop and gdp
  basic <- basic %>%  
    select(ISO,Year,pop_UN,gdp_ppp_WB)
  
  rates <- left_join(rates,basic,by=c("ISO"="ISO","year"="Year"))
  
  rates <- rates %>% 
    mutate(emissions_pc = emissions_total/pop_UN) %>% 
    mutate(emissions_pgdp = emissions_total/gdp_ppp_WB)
  
  region_rates <- rates
  
  # what is 75% of the emissions?
  blarg <- rates %>%
    filter(year==2017)
  threshold <- sum(blarg$emissions_total,na.rm=T)*0.75
  
  ## remove countries below absolute emissions threshold
  
  rates <- rates %>% 
    group_by(country) %>% 
    mutate(rate=(last(emissions_total)/first(emissions_total))^(1/(last(year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(emissions_total)-first(emissions_total)) %>%
    filter(year==2017) 
  
  rates <- rates %>% 
    mutate(pop_UN=ifelse(ISO=="AIR",0,pop_UN)) %>% 
    mutate(gdp_ppp_WB=ifelse(ISO=="AIR",0,gdp_ppp_WB)) %>% 
    mutate(emissions_pc=ifelse(ISO=="AIR",0,emissions_pc)) %>% 
    mutate(emissions_pgdp=ifelse(ISO=="AIR",0,emissions_pgdp)) %>% 
    mutate(pop_UN=ifelse(ISO=="SEA",0,pop_UN)) %>% 
    mutate(gdp_ppp_WB=ifelse(ISO=="SEA",0,gdp_ppp_WB)) %>% 
    mutate(emissions_pc=ifelse(ISO=="SEA",0,emissions_pc)) %>% 
    mutate(emissions_pgdp=ifelse(ISO=="SEA",0,emissions_pgdp)) %>% 
    na.omit() 
  
  rates <- rates %>% 
    ungroup() %>% 
    arrange(desc(emissions_total))
  
  
  rates$cutoff <- 0
  z <- 0
  for (i in 1:length(rates$country)){
    z <- z + rates$emissions_total[i]
    if (z > threshold) {
      break
    }
    rates$cutoff[i] <- 1
  }
  
  country_list <- rates %>% ungroup() %>%  select(ISO,cutoff)
  
  #### calculate growth rates for countries below threshold, grouped by region ####
  
  region_rates <- left_join(region_rates,country_list,by=c("ISO"="ISO"))
  
  region_rates <- region_rates %>% 
    filter(cutoff==0) %>% 
    na.omit %>% 
    group_by(region_ar6_5_short,year) %>% 
    summarise_at(vars(emissions_total,pop_UN,gdp_ppp_WB),sum,na.rm=TRUE)
      
  region_rates <- region_rates %>% 
    mutate(emissions_pc = emissions_total/pop_UN) %>% 
    mutate(emissions_pgdp = emissions_total/gdp_ppp_WB)
    
  region_rates <- region_rates %>% 
    group_by(region_ar6_5_short) %>% 
    mutate(rate=(last(emissions_total)/first(emissions_total))^(1/(last(year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(emissions_total)-first(emissions_total)) %>%
    filter(year==2017) %>% 
    na.omit() %>% 
    mutate(country=paste(region_ar6_5_short,"(rest)")) %>% 
    select(country,everything()) %>% 
    ungroup()
  
  
  ### join and plot
  rates <- rates %>% 
    filter(cutoff==1) %>% 
    select(-cutoff,-ISO)
  
  rates <- rbind(rates,region_rates)
  rates <- rates %>% 
    select(-gdp_ppp_WB,-pop_UN) %>% 
    mutate(rate=rate*100)
  
  rates <- gather(rates,var,value,emissions_pc:abs_growth)
  rates <- rates %>% 
    ungroup()
  
  #rate
  p1 <- rates %>% 
    filter(var=="rate") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value, fill=region_ar6_5_short)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop("Rate of change in" ~.(gas) ~ "Emissions","(%),"~.(time_start)*"-2017"))) +
    #ylab(paste("Rate of change in ",gas," Emissions (%),\n",time_start,"-2017",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank(),
          plot.title = element_text(size = 12)) 
  
  
  #absolute
  p2 <- rates %>% 
    filter(var=="abs_growth") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value/1e6, fill=region_ar6_5_short)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank()) 
  
  #pc
  p3 <- rates %>% 
    filter(var=="emissions_pc") %>% 
    filter(region_ar6_5_short!="AIR") %>% 
    filter(region_ar6_5_short!="SEA") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value, fill=region_ar6_5_short)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop( ~.(gas) ~ "Emissions per capita","(t" ~CO[2]* "eq), 2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank()) 
  
  #pgdp
  p4 <- rates %>% 
    filter(var=="emissions_pgdp") %>% 
    filter(region_ar6_5_short!="AIR") %>% 
    filter(region_ar6_5_short!="SEA") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value, fill=region_ar6_5_short)) +
    geom_bar(stat='identity') + 
    ylab(bquote(atop( ~.(gas) ~ "Emissions per $GDP","(t/$GDP), 2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none",
          axis.title.y = element_blank()) 
  
  
  return(list("rate_plot"=p1,"abs_plot"=p2,"pc_plot"=p3,"pgdp_plot"=p4,"data"=rates))
}
