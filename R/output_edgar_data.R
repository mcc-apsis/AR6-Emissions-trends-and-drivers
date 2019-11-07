
rm(list = ls())
library(tidyverse)
library(ggpubr)
library(gganimate)
library(xlsx)
load('Data/edgar.RData')

#################### Master file, all detail

#write.csv(x = edgar_GHG,file = 'Results/Data/ipcc_ar6_edgar_data_full.csv',row.names = FALSE)

