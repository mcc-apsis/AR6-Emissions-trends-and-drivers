


gwps <- openxlsx::read.xlsx('Data/IPCC emissions data AR6/EDGAR v5.0 Part B--F-gases FT2018 (1970-2018) by JRC and PBL 26Nov2019 for IPCC_WGIII.xlsx',
                            sheet='F-gas (kton)',startRow=10)
gwps <- gwps[63:67]
gwps <- gwps %>% 
  select(-IPCC.gas) %>% 
  unique() %>% 
  select(Fgas,gwp_ar1=GWP_KP,gwp_ar4=`_AR4`,gwp_ar5=`_AR5`)


gwps <- gwps %>% 
  add_row(Fgas="CO2",gwp_ar1=1,gwp_ar4=1,gwp_ar5=1) %>% 
  add_row(Fgas="CH4",gwp_ar1=21,gwp_ar4=25,gwp_ar5=28) %>%
  add_row(Fgas="N2O",gwp_ar1=310,gwp_ar4=298,gwp_ar5=265)

input <- openxlsx::read.xlsx('Data/global_warming_potentials.xlsx',sheet='gwp100')

input <- input %>% 
  mutate(gas=Chemical.formula) %>% 
  mutate(gas=ifelse(is.na(gas),Common.name,gas)) %>% 
  select(gas,gwp_ar6)

gwps <- left_join(gwps,input,by=c("Fgas"="gas")) %>% 
  select(gas=Fgas,everything()) %>% 
  arrange(gwp_ar6)

save(gwps,file='Data/gwps.RData')
