######## decomposition figure (BY COUNTRIES)

gas="GHG"
time_start=2010

decomp_figure_sectors <- function(time_start,gas,edgar_GHG) {
  
  # set palette
  #ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set2") + scale_fill_brewer(palette="Set2")
  
  rates <- edgar_GHG %>%
    filter(year>=time_start & year<2018) %>% 
    group_by(chapter,chapter_title,sector_code,description,year) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    rename_at(gas,funs(sprintf('value')))
  
  # what is x% of the emissions?
  blarg <- rates %>%
    filter(year==2017)
  threshold <- sum(blarg$value,na.rm=T)*0.75
  
  sector_rates <- rates
  ## remove countries below absolute emissions threshold
  
  rates <- rates %>% 
    group_by(sector_code) %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(year==2017) 
  
  rates <- rates %>% 
    ungroup() %>% 
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
  
  ##### add aviation manually
  rates$cutoff[rates$sector_code=="1C1"] <- 1
  rates$cutoff[rates$sector_code=="1A3a"] <- 1
  
  sector_list <- rates %>% select(sector_code,cutoff)
  
  #### sum up rest of the sectors
  
  sector_rates <- left_join(sector_rates,sector_list,by=c("sector_code"="sector_code"))
  
  sector_rates <- sector_rates %>% 
    filter(cutoff==0) %>% 
    na.omit %>% 
    group_by(chapter,chapter_title,year) %>% 
    summarise_at(vars(value),sum,na.rm=TRUE)
  
  sector_rates <- sector_rates %>% 
    group_by(chapter_title) %>% 
    mutate(rate=(last(value)/first(value))^(1/(last(year)-time_start+1))-1) %>% 
    mutate(abs_growth=last(value)-first(value)) %>%
    filter(year==2017) %>% 
    mutate(description=paste(chapter_title,"(rest)")) %>% 
    select(chapter,chapter_title,description,everything()) %>% 
    ungroup()
  
  ### join
  rates <- rates %>% 
    filter(cutoff==1) %>% 
    select(-cutoff,-sector_code)
  
  rates <- rbind(rates,sector_rates) %>% 
    mutate(value=value/1e9) %>% 
    mutate(rate=rate*100)
  
  rates <- gather(rates,var,value,rate:abs_growth)
  rates <- rates %>% 
    ungroup()
  
  rates$chapter_title <- factor(rates$chapter_title,levels(rates$chapter_title)[c(3,1,2,5,4)])
  
  plot_theme <- theme_bw() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 11,color="#737373"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position="none",
          text = element_text(size=11,color="#737373"))
  
  #rate
  p1 <- rates %>% 
    filter(var=="rate") %>% 
    ggplot(.,aes(x = reorder(description,value),y = value, fill=chapter_title)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop("Rate of change in" ~.(gas) ~ "Emissions","(%),"~.(time_start)*"-2017"))) +
    coord_flip() +
    annotate(geom = 'text', label = 'Years: 2010-2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.4,size=3.5,color="#737373") +
    plot_theme +
    ggtitle("GHG emissions growth (%)")
  
  
  #absolute
  p2 <- rates %>% 
    filter(var=="abs_growth") %>% 
    ggplot(.,aes(x = reorder(description,value),y = value/1e9, fill=chapter_title)) +
    geom_bar(stat='identity',colour="#737373") + 
    #ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
    coord_flip() +
    annotate(geom = 'text', label = 'Years: 2010-2017',
             x = -Inf, y = Inf, hjust = 1.05, vjust = -0.4,size=3.5,color="#737373") +
    plot_theme +
    ggtitle("GHG emissions growth (Gt CO2eq)")
  
  return(list("rate_plot"=p1,"abs_plot"=p2,"data"=rates))
         
}


# decomp_figure_sectors <- function(time_start,category,sector,gas,edgar_GHG) {
# 
#   ##############  calculate growth rates in given time period ##############
# 
#   rates <- edgar_GHG %>%
#     filter_at(vars(category),all_vars(.==sector)) %>%
#     filter(!is.na(WB.income)) %>%
#     filter(year>=time_start & year<2018) %>%
#     group_by(sector_code,description,year) %>%
#     summarise_at(gas,sum,na.rm=TRUE) %>%
#     ungroup() %>%
#     rename_at(gas,funs(sprintf('value')))
# 
#   ####### CHECK DATA HERE FOR ZEROS AND NANS #######
# 
#   # what is 75% of the emissions?
#   blarg <- rates %>%
#     filter(year==2017)
#   threshold <- sum(blarg$value,na.rm=T)*0.9
# 
#   ## remove countries below absolute emissions threshold
# 
#   rates <- rates %>%
#     group_by(sector_code) %>%
#     mutate(rate=(last(value)/first(value))^(1/(last(year)-time_start+1))-1) %>%
#     mutate(abs_growth=last(value)-first(value)) %>%
#     filter(year==2017) %>%
#     na.omit() %>%
#     arrange(desc(value))
# 
# 
#   rates$cutoff <- 0
#   z <- 0
#   for (i in 1:length(rates$sector_code)){
#     z <- z + rates$value[i]
#     if (z > threshold) {
#       break
#     }
#     rates$cutoff[i] <- 1
#   }
# 
#   sector_list <- rates %>% select(sector_code,cutoff)
# 
#   #### calculate growth rates for the rest of the sectors
# 
#   blarg <- edgar_GHG %>%
#     filter_at(vars(category),all_vars(.==sector)) %>%
#     filter(!is.na(WB.income)) %>%
#     filter(year>=time_start & year<2018) %>%
#     group_by(sector_code,description,year) %>%
#     summarise_at(gas,sum,na.rm=TRUE) %>%
#     ungroup() %>%
#     rename_at(gas,funs(sprintf('value')))
# 
#   blarg <- left_join(blarg,sector_list,by=c("sector_code"="sector_code")) %>%
#     filter(cutoff==0) %>%
#     filter(year>=time_start & year<2018) %>%
#     group_by(year) %>%
#     summarise_at('value',sum,na.rm=TRUE)
# 
#   blarg <- blarg %>%
#     mutate(rate=(last(value)/first(value))^(1/(last(year)-time_start+1))-1) %>%
#     mutate(abs_growth=last(value)-first(value)) %>%
#     filter(year==2017) %>%
#     na.omit() %>%
#     mutate(sector_code="Rest") %>%
#     mutate(description="Rest")
# 
#   ### join
#   rates <- rbind(rates %>% filter(cutoff==1) %>% select(-cutoff) %>% ungroup(),blarg) %>%
#     mutate(label=paste(sector_code,"-",description))
#   rates <- gather(rates %>% mutate(rate=rate*100),var,value,rate:abs_growth)
# 
#   #rate
#   p1 <- rates %>%
#     filter(var=="rate") %>%
#     ggplot(.,aes(x = reorder(label,value),y = value,fill=label)) +
#     geom_bar(stat='identity') +
#     ylab(bquote(atop("Rate of change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
#     #ylab(paste("Rate of change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
#     coord_flip() +
#     theme_bw() +
#     theme(legend.position="none",
#           axis.title.y = element_blank(),
#           plot.title = element_text(size = 12))
# 
# 
#   #absolute
#   p2 <- rates %>%
#     filter(var=="abs_growth") %>%
#     ggplot(.,aes(x = reorder(label,value),y = value/1e6,fill=label)) +
#     geom_bar(stat='identity') +
#     ylab(bquote(atop("Absolute change in" ~.(gas) ~ "Emissions","(Gt" ~CO[2]* "eq),"~.(time_start)*"-2017"))) +
#     #ylab(paste("Absolute change in ",gas," emissions,\n",time_start,"-2017 (GtCO2eq/yr)",sep="")) +
#     coord_flip() +
#     theme_bw() +
#     theme(legend.position="none",
#           axis.title.y = element_blank())
# 
#   return(list("rate_plot"=p1,"abs_plot"=p2,"data"=rates))


