rm(list = ls())
library(tidyverse)
library(lubridate)

load('Data/edgar_essd_data_ghg_gwp_ar5.RData')


growth_rate <- function(years,y) {
  
  data <- data.frame(years,y)
  
  data <- data %>%
    mutate(leap_years = leap_year(years)) %>%
    mutate(y = ifelse(leap_years==TRUE,y*365/366,y))
  
  fit <- lm(log(y) ~ years,data = data)
  
  # data <- data %>% 
  #   mutate(rate=fit$coefficients[2]) %>% 
  #   mutate(predicted_x = exp(predict(fit,data %>% select(years)))) %>% 
  #   mutate(st_error = sqrt(diag(vcov(fit)))[2])
  # 
  # return(list("rate"=fit$coefficients[2],"data"=data))
  
  return(fit$coefficients[2])
}




subsectors <- edgar_ghg %>% 
  filter(year>2009) %>%
  filter(year<=2019) %>% 
  group_by(year,subsector_title) %>% 
  summarise(value=sum(GHG,na.rm=TRUE))


subsectors <- subsectors %>% 
  group_by(subsector_title) %>% 
  mutate(avg_annual_growth=growth_rate(years = year,y=value)) %>% 
  mutate(avg_annual_growth=avg_annual_growth*100)


subsectors <- subsectors %>% 
  group_by(subsector_title) %>% 
  mutate(abs_growth=(last(value)-first(value))/1e9)


subsectors <- spread(subsectors,year,value) %>% 
  arrange(abs_growth)

openxlsx::write.xlsx(subsectors,"R/Analysis and figures for ESSD paper/Results/Data/fastest_largest_subsectors.xlsx")
