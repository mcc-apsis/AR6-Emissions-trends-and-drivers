index code for later



### Emissions split by sectors with direct and indirect components (SPM.3)

```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Figures/SPM_indirect_emissions.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="direct_indirect"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="direct_indirect"])
cat("<br>")
cat("Links: ")
cat("<a href='https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers/raw/master/Results/Plot%20data/ipcc_ar6_figure_spm2_regions.xlsx'>data</a> | 
    <a href='https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers/blob/master/R/Analysis%20and%20figures/direct_indirect_emissions.Rmd'>code</a> | 
    <a href='https://github.com/mcc-apsis/AR6-Emissions-trends-and-drivers/raw/master/Results/Figures/SPM_indirect_emissions.png'>download png</a>")

```





-----------------------------------------------------------------------
  ### Largest and fastest growing subsectors
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Figures/subsectors_indirect_emissions.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_subsectors"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_subsectors"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/subsectors_indirect_emissions.png.png'>download png</a>")
```



-----------------------------------------------------------------------
  ### Energy sector - per capita levels and kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Sectors/energy_summary-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_energy_summary"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_energy_summary"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/energy_summary-1.png'>download png</a>")
```

-----------------------------------------------------------------------
  ### Energy sector - kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Kaya/energy_systems_10-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_energy_kaya"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_energy_kaya"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Kaya/energy_systems_10-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### Industry sector - per capita levels and kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Sectors/industry_summary-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_industry_summary"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_industry_summary"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/industry_summary-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### Industry sector - kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Kaya/industry_10-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_industry_kaya"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_industry_kaya"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Kaya/industry_10-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### Buildings sector - per capita levels and kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Sectors/buildings_summary-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_buildings_summary"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_buildings_summary"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/buildings_summary-1.png'>download png</a>")
```

-----------------------------------------------------------------------
  ### Buildings sector - kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Kaya/buildings_10-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_buildings_kaya"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_buildings_kaya"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Kaya/buildings_10-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### Transport sector - per capita levels and kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Sectors/transport_summary-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_transport_summary"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_transport_summary"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/transport_summary-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### Transport sector - kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Kaya/transport_10-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_transport_kaya"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_transport_kaya"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Kaya/transport_10-1.png'>download png</a>")
```

-----------------------------------------------------------------------
  ### AFOLU sector - per capita levels and kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Sectors/afolu_summary-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_afolu_summary"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_afolu_summary"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Sectors/afolu_summary-1.png'>download png</a>")
```


-----------------------------------------------------------------------
  ### AFOLU sector - kaya trends
  
  
  ```{r,out.width="80%",fig.align = 'center',results='asis'}
cat("<br>")
include_graphics("Results/Plots/Kaya/AFOLU_hong-1.png")

```
```{r,results = 'asis'}
cat("<br>")
cat("<span class='figure_title'>",captions$title[captions$plot=="sectors_afolu_kaya"],"</span>")
cat("<br>")
cat(captions$caption[captions$plot=="sectors_afolu_kaya"])
cat("<br>")
cat("Links: ")
cat("<a href='https://raw.githubusercontent.com/mcc-apsis/AR6-Emissions-trends-and-drivers/master/Results/Plots/Kaya/AFOLU_hong-1.png'>download png</a>")
```
