#sector=6
#gas="GHG"
#time_start=2010
#category="chapter"

######## bar figure

bar_figure <- function(category,sector,gas,edgar_GHG) {
  
  data <- edgar_GHG %>% 
    filter(year==2017) %>% 
    group_by(chapter) %>% 
    summarise_at(gas,sum) %>% 
    filter(!is.na(chapter)) %>% 
    arrange(desc(GHG))
  
  total = sum(data$GHG)
  
  data <- data %>% 
    mutate(fraction=round(GHG/total*100,2)) %>% 
    mutate(alpha=if_else(chapter==sector,TRUE,FALSE))
  
  data <- data %>% 
    mutate(title=if_else(chapter==6,"Energy","title")) %>% 
    mutate(title=if_else(chapter==7,"AFOLU",title)) %>% 
    mutate(title=if_else(chapter==9,"Bldgs",title)) %>% 
    mutate(title=if_else(chapter==10,"Transport",title)) %>%
    mutate(title=if_else(chapter==11,"Industry",title))
  
  p <- data %>% 
    mutate(fraction=round(fraction)) %>% 
    ggplot(.,aes(y=reorder(fraction,fraction),x=as.factor(1),alpha=alpha)) +
    geom_bar(stat='identity',size=0.5,fill="#252525",color="#737373",width=0.9)  +
    scale_alpha_discrete(range = c(0.25, 0.50)) +
    theme_bw() +
    coord_flip() +
    geom_text(aes(label = paste0(title,"\n(",fraction,"%)")), 
              position = position_stack(vjust = 0.5),alpha=1,fontsize=6) +
    ggtitle("A. Sector contribution to total emissions") +
    theme(legend.position="none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(size = 12))
  
  fraction <- data %>% 
    filter(alpha==TRUE) %>% 
    select(value=fraction)
  
  return(list("plot"=p,"fraction"=fraction))
}


######## table 

make_table <- function(data) {
  
  ft <- flextable(data,col_keys=names(data))
  ft <- autofit(ft)
  ft <- fontsize(ft, size = 8,part="all")
  ft <- height_all(ft,0.1)
  ft <- width(ft,j=3,width=5)
  ft <- align(ft,align="left",part="all")
  
  return(ft)
}


######## location of % shares in stacked figures

locate_shares <- function(data,grouping,years){

  shares <- data %>%
    arrange_at(vars(`year`,grouping)) %>%
    mutate(location=value/2)
  
  z = nrow(unique(shares[,grouping]))
  
  for (j in seq(0,z*(years-1),z)) {
    # for every region
    for (i in 1:z) {
      if (i != z) {
        shares$location[i+j] = shares$location[i+j] + sum(shares$value[i+1+j:(z-i+j-1)])
        #shares$location[i] = shares$location[i] + sum(shares$value[i+1:(z-i))])
        
      }
    }
  }

  return(shares)
}


######## set themes

big_trend_theme <- theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 12))


addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}



######## trend figure

time_start=2010
category = "sector_code"
sector="Total"
gas="GHG"
region="region_ar6_5"




trend_figure <- function(time_start,category,sector,gas,edgar_GHG,big_trend_theme) {
  
  data <- totals %>%
    filter_at(vars(category),all_vars(.==sector)) %>% 
    filter(!is.na(region_ar6_5)) %>%
    group_by(year,region_ar6_5) %>% 
    summarise_at(gas,sum,na.rm=TRUE) %>% 
    rename_at(gas,funs(sprintf('value'))) %>% 
    mutate(value=value/1e6)
  
  
  
  # shares <- data %>% 
  #   filter(year %in% c(1990,2000,2010,2017)) %>% 
  #   group_by(year) %>% 
  #   mutate(totals=sum(value)) %>% 
  #   ungroup() %>% 
  #   group_by(year,region_ar6_5) %>%  
  #   mutate(fractions=(value/totals)*100)
  # 
  # shares <- shares %>%
  #   arrange(year) %>%
  #   mutate(location=value/2)
  # 
  # z = length(unique(shares$region_ar6_5))
  # 
  # for (j in seq(0,z*4,z)) {
  #  # for every region
  #   for (i in 1:z) {
  #       if (i != seq(z,z*4,z)) {
  #         shares$location[i+j] = shares$location[i+j] + sum(shares$value[i+1+j:(z-i+j-1)])
  #         #shares$location[i] = shares$location[i] + sum(shares$value[i+1:(z-i))])
  # 
  #       }
  #   }
  # }
  
  
  # for (i in seq(1,24,4)) {
  #   
  #   shares$location[i] = (shares$value[i]/2) + shares$value[i+1] + shares$value[i+2] + shares$value[i+3]
  #   shares$location[i+1] = (shares$value[i+1]/2) + shares$value[i+2] + shares$value[i+3]
  #   shares$location[i+2] = (shares$value[i+2]/2) + shares$value[i+3]
  #   shares$location[i+3] = (shares$value[i+3]/2)
  #   
  # }
  
  p <- data %>%
    ggplot(.,aes(x=year,y=value,fill=region_ar6_5)) +
    geom_area(color='#737373') +
    #geom_text(data=shares,aes(x=year,y=location,label=paste(round(fractions,0),"%",sep="")))+
    geom_vline(xintercept=c(1990,2000,2010,2017),alpha=0.3,linetype="dashed") +
    big_trend_theme +
    ggtitle("B. Sector trend by income group") +
    theme(legend.justification=c(0,1), 
          legend.position=c(0.05, 0.97),
          legend.title=element_blank(),
          legend.background = element_blank(),
          legend.spacing.x = unit(0.2,'cm')) +
    ylab(bquote(.(gas) ~" Emissions (Gt" ~CO[2]* "eq)"))
  
  return(list("plot"=p,"data"=data))
  
}
