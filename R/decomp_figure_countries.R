######## decomposition figure (BY COUNTRIES)

# time_start=2010
# gas='GHG'
# edgar_GHG <- edgar_GHG_ar5

decomp_figure_countries <- function(time_start,gas,edgar_GHG,basic) {
  
  ipcc_palette <- c("#F8766D","#c49a00","#53b400","#00c094","#00b6eb","#a58aff","#fb61d7");
  
  ##############  calculate growth rates in given time period ############## 
  
  rates <- edgar_GHG %>% 
    filter(region_ar6_5_short!="AIR") %>% 
    filter(region_ar6_5_short!="SEA") %>% 
    filter(year>=time_start & year<2019) %>% 
    group_by(country,ISO,year,region_ar6_5_short) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    ungroup() %>% 
    rename_at(gas,funs(sprintf('emissions_total')))
  
  ####### CHECK DATA HERE FOR ZEROS AND NANS ####### 
  
  ### join pop and gdp
  basic <- basic %>%  
    filter(Year>=time_start & Year<2019) %>% 
    select(ISO,Year,pop_UN,gdp_ppp_WB)
  
  ####################################### BRING FORWARD MISSING GDP DATA
  basic <- basic %>% 
    group_by(ISO) %>% 
    fill(gdp_ppp_WB)
  
  rates <- left_join(rates,basic,by=c("ISO"="ISO","year"="Year"))
  
  rates <- rates %>% 
    mutate(emissions_pc = emissions_total/pop_UN) %>% 
    mutate(emissions_pgdp = emissions_total/gdp_ppp_WB)
  
  region_rates <- rates
  
  # what is 75% of the emissions?
  blarg <- rates %>%
    filter(year==2018)
  threshold <- sum(blarg$emissions_total,na.rm=T)*0.75
  
  ## remove countries below absolute emissions threshold
  
  rates <- rates %>% 
    group_by(country) %>% 
    mutate(rate=(last(emissions_total)/first(emissions_total))^(1/(last(year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(emissions_total)-first(emissions_total)) %>%
    filter(year==2018) 
  
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
    filter(year==2018) %>% 
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
  
  plot_theme <- theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 11,color="#737373"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="none",
          text = element_text(size=11,color="#737373"))
  
  
  library(forcats)
  
  rates$region_ar6_5_short <- lvls_expand(rates$region_ar6_5_short,c(levels(rates$region_ar6_5_short),"LAND"))
  rates$region_ar6_5_short <-  factor(rates$region_ar6_5_short,levels(rates$region_ar6_5_short)[c(3,4,5,6,7,1,8)])
  
  
  
  #rate
  p1 <- rates %>% 
    filter(var=="rate") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value, fill=region_ar6_5_short)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop("Rate of change in" ~.(gas) ~ "Emissions","(%),"~.(time_start)*"-2017"))) +
    #ylab(paste("Rate of change in ",gas," Emissions (%),\n",time_start,"-2017",sep="")) +
    coord_flip() +
    scale_fill_manual(values = ipcc_palette) +
    annotate(geom = 'text', label = 'Years: 2010-2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.4,size=3.5,color="#737373") +
    plot_theme +
    ggtitle("GHG emissions growth (%)")
    
  
  #absolute
  p2 <- rates %>% 
    filter(var=="abs_growth") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value/1e9, fill=region_ar6_5_short)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    scale_fill_manual(values = ipcc_palette) +
    plot_theme +
    ggtitle("GHG emissions growth (Gt CO2eq)") +
    annotate(geom = 'text', label = 'Years: 2010-2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.3,size=3.5,color="#737373")
  
  #pc
  p3 <- rates %>% 
    filter(var=="emissions_pc") %>%  
    ggplot(.,aes(x = reorder(country,value),y = value, fill=region_ar6_5_short)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop( ~.(gas) ~ "Emissions per capita","(t" ~CO[2]* "eq), 2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    scale_fill_manual(values = ipcc_palette) +
    plot_theme +
    ggtitle("GHG emissions per capita (tCO2/cap)") +
    annotate(geom = 'text', label = 'Year: 2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.3,size=3.5,color="#737373")
  
  #pgdp
  p4 <- rates %>% 
    filter(var=="emissions_pgdp") %>% 
    ggplot(.,aes(x = reorder(country,value),y = value*1000, fill=region_ar6_5_short)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop( ~.(gas) ~ "Emissions per $GDP","(t/$GDP), 2017"))) +
    #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
    coord_flip() +
    scale_fill_manual(values = ipcc_palette) +
    plot_theme +
    ggtitle("GHG emissions intensity (kgCO2/$)") +
    annotate(geom = 'text', label = 'Year: 2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.3,size=3.5,color="#737373")
  
  
  
  return(list("rate_plot"=p1,"abs_plot"=p2,"pc_plot"=p3,"pgdp_plot"=p4,"data"=rates))
}
