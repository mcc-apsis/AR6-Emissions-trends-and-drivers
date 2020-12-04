growth_rate <- function(years,y) {
  
  data <- data.frame(years,y)
  
  fit <- lm(log(y) ~ years,data = data)
  
  data <- data %>% 
    mutate(rate=fit$coefficients[2]) %>% 
    mutate(predicted_x = exp(predict(fit,data %>% select(years)))) %>% 
    mutate(st_error = sqrt(diag(vcov(fit)))[2])
  
  return(list("rate"=fit$coefficients[2],"data"=data))
}