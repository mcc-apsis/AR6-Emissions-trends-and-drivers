
# rm(list = ls())
# library(tidyverse)
# 
# 
# load('Data/edgar_data_gwp_ar6.RData')
# 
# time_start=1970
# 
# usa <- edgar_GHG_ar6 %>%
#   filter(country=="United Kingdom") %>%
#   group_by(year) %>%
#   summarise(GHG=sum(GHG,na.rm=TRUE)) %>%
#   filter(year>=time_start)
# 
# 
# usa <- usa %>%
#   mutate(rate_linear=(last(GHG)/first(GHG))^(1/(last(year)-first(year)))-1)
# 
# 
# asd <- aagr(usa,"GHG")

# aagr <- function(data,var) {
#   
#   data <- data %>% 
#     mutate(leap_years = leap_year(years)) %>% 
#     mutate(adjusted = ifelse(leap_years==TRUE,!!as.name(var)*365/366,!!as.name(var)))
#   
#   fit <- lm(log(adjusted) ~ year,data = data)
#   
#   data <- data %>% 
#     mutate(rate=fit$coefficients[2]) %>% 
#     mutate(predicted_x = exp(predict(fit,data %>% select(year))))
#   
#   return(data)
# }
# 
# asd <- growth_rate(usa$year,usa$GHG)
# years <- usa$year
# y=usa$GHG

library(lubridate)

growth_rate <- function(years,y) {
  
  data <- data.frame(years,y)
  
  # data <- data %>% 
  #   mutate(leap_years = leap_year(years)) %>% 
  #   mutate(adjusted = ifelse(leap_years==TRUE,y*365/366,y))
  data <- data %>% 
    mutate(adjusted=y)
  
  fit <- lm(log(adjusted) ~ years,data = data)
  
  data <- data %>% 
    mutate(rate=fit$coefficients[2]) %>% 
    mutate(predicted_x = exp(predict(fit,data %>% select(years)))) %>% 
    mutate(st_error = sqrt(diag(vcov(fit)))[2])
  
  return(list("rate"=fit$coefficients[2],"data"=data))
}


# 
# ggplot(asd,aes(y=GHG,x=year)) + 
#   geom_path()+
#   geom_path(aes(x=year,y=predicted_x),color="red")




################## Code from Robbie and Glen (Matlab)

# %% AAGR - annual average growth rate
# %
# % Basic usage:
#   %  g = aagr(yrs,data)
# %
# %  Full usage:
#   %    [g,bint,r,rint,stats] = aagr(yrs,data,period,conf,bLeap)
# %
# %    yrs - time variable, years (vector, length n)
# %   data - data (vector, length n)
# % period - start and end year over which to calculate growth from available data (vector, length 2)
# %   conf - confidence interval for returned statistics, if any (passed to regress, default 95)
# %  bLeap - treatment of leap years: 0: no adjustment (default), 1: adjustment
# %
# % Note on AAGR
# % The average annual growth rate is not consistently defined in the literature. We choose here to
# % calculate it as the best fit of an exponential to the growing data series. The result is the
# % annual growth rate that, if compounded continuously, would produce an exponential that best fits
# % those data.
# %
# % To demonstrate this, try the following, which shows the difference between the fitted AAGR and e
# % for ever-smaller compounding periods:
#   % x = 1:20 ;
# % for n=10.^(0:5) % n is the number of compounding periods
# %     y = (1+exp(1)/100/n).^(1:n:(20*n)) ;
# %     fprintf('%6d: %1.6f\n',n,aagr(x,y)*100-exp(1))
# % end
# %
# % Robbie Andrew, CICERO, 2014
# % Modified to permit non-continuous data, Glen Peters October 2014
# 
# function [m,mint,n,nint,stats] = aagr(yrs,data,period,conf,bLeap)
# 
# if numel(yrs)~=numel(data)
#     error('time and data vectors are inconsistent')
# end
# 
# data = data(:) ; % to column vector
# yrs = yrs(:) ;
# 
# if nargin<3 || isempty(period)
#     period = [yrs(1) yrs(end)] ; end
# if nargin<4 || isempty(conf)
#     conf = 95 ; end
# 
# if nargin>4 && bLeap
#     leap = mod(yrs,400)==0 | (mod(yrs,100)~=0 & mod(yrs,4)==0) ;
#     data(leap)=data(leap)*365/366 ;
# end
# 
# sy = period(1) ;
# ey = period(end) ;
# 
# i = find(yrs==sy):find(yrs==ey) ;
# x = yrs(i);
# y = data(i);
# if any(y==0)
#     warning('AAGR:FoundZeros','Found zeros in y; forcing to 1e-9')
#     y(y==0) = 1e-8 ;
# end
# if ~all(isnan(y))
#     [m,BINT,R,RINT,STATS] = regress(log(y),[ones(size(x)) x],1-conf/100) ;
# else
#     m = [NaN NaN] ; BINT = NaN ; R = NaN ; RINT = NaN ; STATS = NaN ;
# end
# 
# if nargout<2
#     m = m(2) ;
# end
# 
# if nargout>1
#     mint=BINT ; end
# if nargout>2
#     n=R; end
# if nargout>3
#     nint=RINT ; end
# if nargout>4
#     stats=STATS ; end
# 
# 
