rm(list = ls())
library(tidyverse)

ipcc_sectors <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDITABLE.xlsx',
                               'ar6_sector_classification')


ipcc_sectors <- ipcc_sectors %>% 
  select(code:description,subsector=subsector_suggestion_TSU,subsector_title=subsector_title_suggestion_TSU)

save(ipcc_sectors,file='Data/ipcc_sectors.RData')

