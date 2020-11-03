
choice="gwp_ar6"


waterfall_plot <- function(plot_data,gwps,choice,uncertainties,labels) {
  
  
  waterfall <- plot_data %>%
    filter(year == 2018) %>%
    group_by(gas) %>%
    summarise(value = sum(value)) %>%
    arrange(desc(gas))
  
  waterfall <- left_join(waterfall,gwps,by=c("gas"="gas"))
  
  waterfall <- waterfall %>% 
    mutate(ar5=gwp_ar5) %>% 
    select_at(vars(sprintf('gas'),sprintf('value'),sprintf('ar5'),choice=choice))
  
  waterfall <- waterfall %>% 
    mutate(newvalue=value) %>% 
    mutate(newvalue=ifelse(gas=="CH4",(newvalue/ar5)*choice,newvalue)) %>% 
    mutate(newvalue=ifelse(gas=="N2O",(newvalue/ar5)*choice,newvalue))
  
  waterfall$end <- cumsum(waterfall$newvalue)
  waterfall$start <- c(0, head(waterfall$end, -1))
  waterfall <- cbind(waterfall,id=c(5,4,3,2,1))
  
  
  ###### uncertainties
  
  waterfall <- left_join(waterfall,uncertainties,by=c("gas"="gas"))
  waterfall <- waterfall %>% 
    mutate(abs_uncertainty=newvalue*uncertainty) %>% 
    mutate(uncertainty_start=end-(0.5*abs_uncertainty)) %>% 
    mutate(uncertainty_end=end+(0.5*abs_uncertainty)) %>% 
    mutate(totals = sum(waterfall$newvalue))
  
  
  waterfall$gas <- as.factor(waterfall$gas)
  waterfall$gas <-  factor(waterfall$gas,levels(waterfall$gas)[c(4,5,1,3,2)])
  
  if (labels=="no") {
    
    p <- waterfall %>%
      ggplot(., aes(fill = gas)) +
      geom_rect(aes(
        x = gas,
        xmin = id - 0.4,
        xmax = id + 0.4,
        ymin = start,
        ymax = end
      ),colour = "#737373") +
      
      geom_rect(aes(
        xmin = id,
        xmax = id,
        ymin = uncertainty_start,
        ymax = uncertainty_end
      ),colour = "#252525") +
      
      geom_rect(aes(
        xmin = id - 0.2,
        xmax = id + 0.2,
        ymin = uncertainty_start,
        ymax = uncertainty_start
      ),colour = "#252525") +
      
      geom_rect(aes(
        xmin = id - 0.2,
        xmax = id + 0.2,
        ymin = uncertainty_end,
        ymax = uncertainty_end
      ),colour = "#252525") +
      
      geom_text(data=waterfall %>% filter(gas=="CH4"),aes(x=gas,y=62,label=paste(round(totals,0),  "Gt")),size=3.5,colour="#252525")+
      
      
      
      theme_bw() +
      ylim(0, 62) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )
    
  }
  else {
    
    p <- waterfall %>%
      ggplot(., aes(fill = gas)) +
      geom_rect(aes(
        x = gas,
        xmin = id - 0.4,
        xmax = id + 0.4,
        ymin = start,
        ymax = end
      ),colour = "#737373") +
      
      geom_rect(aes(
        xmin = id,
        xmax = id,
        ymin = uncertainty_start,
        ymax = uncertainty_end
      ),colour = "#252525") +
      
      geom_rect(aes(
        xmin = id - 0.2,
        xmax = id + 0.2,
        ymin = uncertainty_start,
        ymax = uncertainty_start
      ),colour = "#252525") +
      
      geom_rect(aes(
        xmin = id - 0.2,
        xmax = id + 0.2,
        ymin = uncertainty_end,
        ymax = uncertainty_end
      ),colour = "#252525") +
      
      geom_text(data=waterfall %>% filter(gas=="CH4"),aes(x=gas,y=62,label=paste(round(totals,0),  "Gt")),size=3.5,colour="#252525")+
      
      
      
      theme_bw() +
      ylim(0, 62) +
      theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank()
      )
    
    
  }
  
  
  return(list(
    "plot" = p,
    "data" = waterfall
  ))
}
