
rm(list = ls())
library(xlsx)
library(tidyverse)


my_sheet <- read.xlsx('Data/IPCC_master_categories.xlsx',sheetName='full_list')

thing <- read.xlsx('Data/IPCC Mapping.xlsx',sheetName='EmissionMap',startRow = 3,endRow = 132) %>% 
  select(code=IPCC.cat.,IPCC_AR5_sector=Sector.1,IPCC_AR5_chapter=Chapter.1,IPCC_AR5_description=IPCC_description)

z <- full_join(my_sheet,thing,by=("code"="code")) %>% 
  select(code,IPCC_1996_description,EDGAR_description,IPCC_AR5_description,IPCC_AR5_sector,IPCC_AR5_chapter)
  arrange(code)

write.xlsx(z,'Data/IPCC_master_categories.xlsx',sheetName='code_comparisons',row.names = FALSE,append=TRUE)
  