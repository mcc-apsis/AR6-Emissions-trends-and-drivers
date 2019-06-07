
rm(list = ls())
library(xlsx)
library(tidyverse)

##### import data from matlab

file.copy('C:/Users/lamw/Documents/SpiderOak Hive/Work/Code/MATLAB/Data shop/Aggregation/Basic data/basic.xls','Data/',overwrite=TRUE)

basic<-read.xlsx('Data/basic.xls','data_full')
save(basic,file='Data/basic.RData')


#z<- read.delim('Data/labels_T.txt',sep = '\t',header = FALSE)
#blarg <- data.frame(unique(z$V1))
