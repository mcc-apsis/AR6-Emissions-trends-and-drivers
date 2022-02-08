growth_rate <- function(data) {
  
  # leap year adjustment
  # rates <- data %>%
  #   mutate(leap_years = leap_year(year)) %>%
  #   mutate(value = ifelse(leap_years==TRUE,value*365/366,value)) %>%
  #   select(-leap_years)
  
  rates <- data %>% 
    filter(year %in% c(1990,1999,2000,2009,2010,end_year)) %>% 
    mutate(total_rate=NA)
  
  rates$total_rate[rates$year==1990] = ((rates$value[rates$year==1999]/rates$value[rates$year==1990])^(1/(1999-1990))-1)*100
  
  rates$total_rate[rates$year==2000] = ((rates$value[rates$year==2009]/rates$value[rates$year==2000])^(1/(2009-2000))-1)*100
  
  rates$total_rate[rates$year==2010] = ((rates$value[rates$year==end_year]/rates$value[rates$year==2010])^(1/(end_year-2010))-1)*100
  
  
  return(rates)
  
}