rm(list = ls())
library(tidyverse)

#ipcc_sectors <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_EDITABLE.xlsx',
#                                'ar6_sector_classification')
# 
# ipcc_sectors <- ipcc_sectors %>% 
#   select(code:description,subsector=subsector_suggestion_Lamb,subsector_title=subsector_title_suggestion_Lamb)

ipcc_sectors <- openxlsx::read.xlsx('Data/Codes and classifications/sector_classification_revised_v1.xlsx',
                               'ar6_sector_classification')

ipcc_sectors <- ipcc_sectors %>%
  select(code:description,subsector=subsector_ar6,subsector_title=subsector_title_ar6)


save(ipcc_sectors,file='Data/ipcc_sectors.RData')

