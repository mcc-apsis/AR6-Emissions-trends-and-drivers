library(tidyverse)


gwps <- openxlsx::read.xlsx('Data/Codes and classifications/gwps.xlsx','gwps')

gwps_ch4 <- openxlsx::read.xlsx('Data/Codes and classifications/gwps.xlsx','ch4')




save(gwps,gwps_ch4,file='Data/gwps.RData')





# what is biogenic?

# load('Data/edgar_data_gwp_ar6.RData')
# blarg <- edgar_GHG_ar6 %>% 
#   group_by(chapter,chapter_title,subsector,sector_code,description) %>% 
#   summarise(CH4=sum(CH4,na.rm=TRUE)) #%>% 
# 
# blarg <- blarg %>% 
#   filter(CH4!=0)
# 
# openxlsx::write.xlsx(x=blarg,file="CH4_sources.xlsx",sheetName = "data",row.names=FALSE)
