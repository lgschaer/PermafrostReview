## This is the code repository for "Permafrost microbes unleashed: thaw reactors provide timely insights into greenhouse gas feedbacks for climate stewardship"

### Files include:

##### Clean_Code_for_Review_Figs_V2.R Script to make Figure 2 (an upset plot using the R package complexUpset) and code for a meta analysis of carbon dioxide and methane flux data

##### ord_vars_for_R_V4.csv Input data for Figure 2

##### flux_data_compilation.csv Input data for the meta analysis

### Additional information about the meta analysis of flux data is available on zenodo: [10.5281/zenodo.10914506](https://zenodo.org/doi/10.5281/zenodo.10914505)

---
Meta-Analysis of data using metafor
by Laura Schaerer

See Meta-Analysis-Report-V1.pdf for outputs from the code
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Meta-Analysis for Permafrost Review: 

```{r packages}
#load required R packages
library(tidyverse)
library(metafor)
library(Matrix)
```

For all analyses obtained mean and standard error from figures using metaDigitise and compiled into a single spreadsheet for each analysis.


### CO2 emissions from Active Layer vs Permafrost by temperature

Including studies that incubated Active Layer and Permafrost samples at one or more temperatures and measured CO2 emissions. 

Studies included from review: Doherty, Dutta, Gentsch, Treat, Roy Chowdhury, Bracho, Jiang, Song 2023, Moni, Muller, Mackelprang 2011.


```{r wrangle}
#load data
co2_flux_by_layer_temp <- read_csv("/Users/lgschaer/Desktop/MetaA_Figs/co2_flux_by_layer_temp.csv")

#wrangle data into necessary format
metaA <- co2_flux_by_layer_temp %>%
  group_by(Author, Layer, Temperature) %>%
  summarize(
    mean = mean(mean),
    sd = sqrt(sum(((n-1)*(sd^2)), na.rm = TRUE)/(sum(n)-n())),
    n = sum(n)) %>%
  mutate(Layer1 = Layer,
         Layer2 = Layer) %>%
  ungroup() %>%
  pivot_wider(names_from = "Layer", values_from = "mean", names_prefix = "mean_") %>%
  pivot_wider(names_from = "Layer1", values_from = "sd", names_prefix = "sd_") %>%
  pivot_wider(names_from = "Layer2", values_from = "n", names_prefix = "n_") %>%
  group_by(Author, Temperature) %>%
  reframe( #this works because there is only one observation for each group, doing this to get rid of NAs, not actually adding or subtracting anything here!
    mean_Active_Layer = sum(mean_Active_Layer, na.rm = TRUE),
    mean_Permafrost = sum(mean_Permafrost, na.rm = TRUE),
    sd_Active_Layer = sum(sd_Active_Layer, na.rm = TRUE),
    sd_Permafrost = sum(sd_Permafrost, na.rm = TRUE),
    n_Active_Layer = sum(n_Active_Layer, na.rm = TRUE),
    n_Permafrost = sum(n_Permafrost, na.rm = TRUE)
  )

#use escalc function to calculate effect sizes
##NOTE: I also did this using measure = "SMDH" to account for heteroskedasticity in the data, results were exactly the same
effects <- escalc(measure = "SMD", m1i = mean_Active_Layer, m2i = mean_Permafrost,
       sd1i = sd_Active_Layer, sd2i = sd_Permafrost,
       n1i = n_Active_Layer, n2i = n_Permafrost, data = metaA)
head(effects)
```



SMD = 0 ~ There is no difference in CO2 emissions between Active Layer and Permafrost treatments

SMD < 0 ~ Mean of CO2 emissions from Active Layer is lower than mean of CO2 emissions from Permafrost

SMD > 0 ~ Mean of CO2 emissions from Permafrost is lower than mean of CO2 emissions from Active Layer 

```{r plot, echo=FALSE}
#fit mixed-effects model with temperature as a moderator, super high p-value, seems temp doesn't have a big effect here
#plot it
forest(effects$yi, effects$vi, slab = paste0(effects$Author, " ", effects$Temperature, " C"), header=TRUE, top=2)

```

The figure shows variance in the responses, some studies/temperatures have higher CO2 fluxes from the active layer and other studies/temperatures have higher CO2 fluxes from the permafrost layer, there are no visual patterns. 

```{r fit}
#fit mixed-effects model with temperature as a moderator
### NOTE: super high p-value, seems temp doesn't have a statistically significant effect here
res <- rma(yi, vi, mods = ~ Temperature, slab=Author, data=effects)
res
```

The model fit to temperature is insignificant, suggesting that temperature does not impact fluxes in a consistent way and there are likely other controlling factors that are not accounted for here. 

# CO2 emissions from Active Layer vs Permafrost by incubation time

Including Dutta, Jiang, Moni, Treat, Waldrop 2010, and Roy Chowdhury.


```{r 2}
#load data
co2_flux_by_layer_time <- read_csv("/Users/lgschaer/Desktop/MetaA_Figs/co2_flux_by_layer_time.csv")

#wrangle data into necessary format
metaA <- co2_flux_by_layer_time %>%
  group_by(Article, Layer, Days) %>%
  summarize(
    mean = mean(mean, na.rm = TRUE),
    sd = sqrt(sum(((n-1)*(sd^2)), na.rm = TRUE)/(sum(n)-n())),
    n = sum(n, na.rm = TRUE)) %>%
  mutate(Layer1 = Layer,
         Layer2 = Layer) %>%
  ungroup() %>%
  pivot_wider(names_from = "Layer", values_from = "mean", names_prefix = "mean_") %>%
  pivot_wider(names_from = "Layer1", values_from = "sd", names_prefix = "sd_") %>%
  pivot_wider(names_from = "Layer2", values_from = "n", names_prefix = "n_") %>%
  group_by(Article, Days) %>%
  reframe( 
    mean_ActiveLayer = mean(mean_ActiveLayer, na.rm = TRUE),
    n_ActiveLayer = sum(n_ActiveLayer, na.rm = TRUE),
    sd_ActiveLayer = sqrt(sum(((n_ActiveLayer-1)*(sd_ActiveLayer^2)), na.rm = TRUE)/(sum(n_ActiveLayer)-n())),
    mean_Permafrost = mean(mean_Permafrost, na.rm = TRUE),
    n_Permafrost = sum(n_Permafrost, na.rm = TRUE),
    sd_Permafrost = sqrt(sum(((n_Permafrost-1)*(sd_Permafrost^2)), na.rm = TRUE)/(sum(n_Permafrost)-n()))
  )

#use escalc function to calculate effect sizes
##NOTE: I also did this using measure = "SMDH" to account for heteroskedasticity in the data, results were exactly the same
effects <- escalc(measure = "SMD", m1i = mean_ActiveLayer, m2i = mean_Permafrost,
                  sd1i = sd_ActiveLayer, sd2i = sd_Permafrost,
                  n1i = n_ActiveLayer, n2i = n_Permafrost, data = metaA)
head(effects)
```


SMD = 0 ~ There is no difference in CO2 emissions between Active Layer and Permafrost treatments

SMD < 0 ~ Mean of CO2 emissions from Active Layer is lower than mean of CO2 emissions from Permafrost

SMD > 0 ~ Mean of CO2 emissions from Permafrost is lower than mean of CO2 emissions from Active Layer 
```{r 4}
forest(effects$yi, effects$vi, slab = paste0(effects$Article, " ", effects$Days, " days"), header=TRUE, top=2)
```


Since there is an outlier, visualize with those points removed

```{r 5}
effects2 <- effects %>% filter(Article != "Roy Chowdhury" | Days != 50)
forest(effects2$yi, effects2$vi, slab = paste0(effects2$Article, " ", effects2$Days, " days"), header=TRUE, top=2)
```


Based on the figure, the effect size seems to vary more based on each study than by incubation time.

Check for statistically significant relationships with time.

```{r 7}
res1 <- rma(yi, vi, mods = ~ Days, slab=Article, data=effects)
res1
res2 <- rma(yi, vi, mods = ~ Days, slab=Article, data=effects2)
res2 #significant when the outlier is removed
```

The relationship with time is statistically significant if the outlier points (from the last time point in Roy Chowdhury et al) are removed. 

# CO2 emissions from Aerobic vs Anaerobic incubations

Including studies that incubated samples under both aerobic and anaerobic conditions and measured CO2 emissions. 

Studies included: Capek 2015, Ernakovich 2017, Kane 2013, Knoblauch 2013, Knoblauch 2018, and Lee 2012

Obtained mean and standard error from figures using metaDigitise and compiled into a single spreadsheet.


```{r wrangle2}
#load data
co2_flux_by_headspace_temp <- read_csv("/Users/lgschaer/Desktop/MetaA_Figs/co2_flux_by_headspace_temp.csv")

#wrangle data into necessary format
metaA <- co2_flux_by_headspace_temp %>%
  group_by(Article, Headspace, Temperature) %>%
  summarize(
    mean = mean(mean, na.rm = TRUE),
    n = sum(n, na.rm = TRUE),
    sd = sqrt(sum(((n-1)*(sd^2)), na.rm = TRUE)/(sum(n)-n()))) %>%
  mutate(Headspace1 = Headspace,
         Headspace2 = Headspace,
         manual_sd = case_when(Article == "Ernakovich 2017" & Headspace == "Aerobic" & Temperature == 1 ~ 121.9947,
                        Article == "Ernakovich 2017" & Headspace == "Aerobic" & Temperature == 15 ~ 319.2053,
                        Article == "Ernakovich 2017" & Headspace == "Anaerobic" & Temperature == 1 ~ 61.6669,
                        Article == "Ernakovich 2017" & Headspace == "Anaerobic" & Temperature == 15 ~ 169.8947,
                        Article == "Knoblauch 2018" & Headspace == "Aerobic" ~ 61.3674,
                        Article == "Knoblauch 2018" & Headspace == "Anaerobic" ~ 11.6623),
         sd = ifelse(is.na(sd), manual_sd, sd)
         ) %>%
  ungroup() %>%
  pivot_wider(names_from = "Headspace", values_from = "mean", names_prefix = "mean_") %>%
  pivot_wider(names_from = "Headspace1", values_from = "sd", names_prefix = "sd_") %>%
  pivot_wider(names_from = "Headspace2", values_from = "n", names_prefix = "n_") %>%
  group_by(Article, Temperature) %>%
  reframe( #this works because there is only one observation for each group, doing this to get rid of NAs, not actually adding anything here!
    mean_Aerobic = sum(mean_Aerobic, na.rm = TRUE),
    mean_Anaerobic = sum(mean_Anaerobic, na.rm = TRUE),
    sd_Aerobic = sum(sd_Aerobic, na.rm = TRUE),
    sd_Anaerobic = sum(sd_Anaerobic, na.rm = TRUE),
    n_Aerobic = sum(n_Aerobic, na.rm = TRUE),
    n_Anaerobic = sum(n_Anaerobic, na.rm = TRUE)
  )

#use escalc function to calculate effect sizes
##NOTE: I also did this using measure = "SMDH" to account for heteroskedasticity in the data, results were exactly the same
effects <- escalc(measure = "SMD", m1i = mean_Aerobic, m2i = mean_Anaerobic,
                  sd1i = sd_Aerobic, sd2i = sd_Anaerobic,
                  n1i = n_Aerobic, n2i = n_Anaerobic, data = metaA)
head(effects)
```

SMD = 0 ~ There is no difference in CO2 emissions between Aerobic and Anaerobic treatments

SMD < 0 ~ Mean CO2 emissions from Aerobic Treatments are lower than mean of CO2 emissions from Anaerobic Treatments

SMD > 0 ~ Mean CO2 emissions from Anaerobic Treatments are lower than mean of CO2 emissions from Aerobic Treatments

```{r plot2, echo=FALSE}
#fit mixed-effects model with temperature as a moderator, super high p-value, seems temp doesn't have a big effect here
#plot it
forest(effects$yi, effects$vi, slab = paste0(effects$Article, " ", effects$Temperature, "C"), header=TRUE, top=2)

```

Most studies have positive effect sizes for all temperatures (except for Kane et al 2013), suggesting that higher CO2 emissions come from Aerobic treatments on average.

```{r fit2}
#fit mixed-effects model with temperature as a moderator
### NOTE: super high p-value, seems temp doesn't have a statistically significant effect here
res <- rma(yi, vi, mods = ~ Temperature, slab=Article, data=effects)
res
```

Temperature has a statistically non-significant relationship with effect sizes.

# CH4 emissions from Active Layer vs Permafrost incubations by temperature

```{r A}

#load data
ch4_flux_by_layer_temp <- read_csv("/Users/lgschaer/Desktop/MetaA_Figs/ch4_flux_by_layer_temp.csv")

#wrangle data into necessary format
metaA <- ch4_flux_by_layer_temp %>%
  mutate(Layer2 = Layer,
         Layer3 = Layer) %>%
  pivot_wider(names_from = "Layer", values_from = "mean", names_prefix = "mean_") %>%
  pivot_wider(names_from = "Layer2", values_from = "sd", names_prefix = "sd_") %>%
  pivot_wider(names_from = "Layer3", values_from = "n", names_prefix = "n_") %>%
  group_by(Article, Temperature) %>%
  summarize(
    mean_ActiveLayer = mean(mean_ActiveLayer, na.rm = TRUE),
    n_ActiveLayer = sum(n_ActiveLayer, na.rm = TRUE),
    sd_ActiveLayer = sqrt(sum(((n_ActiveLayer-1)*(sd_ActiveLayer^2)), na.rm = TRUE)/(sum(n_ActiveLayer)-n())),
    mean_Permafrost = mean(mean_Permafrost, na.rm = TRUE),
    n_Permafrost = sum(n_Permafrost, na.rm = TRUE),
    sd_Permafrost = sqrt(sum(((n_Permafrost-1)*(sd_Permafrost^2)), na.rm = TRUE)/(sum(n_Permafrost)-n()))
    )

#use escalc function to calculate effect sizes
##NOTE: I also did this using measure = "SMDH" to account for heteroskedasticity in the data, results were exactly the same
effects <- escalc(measure = "SMD", m1i = mean_ActiveLayer, m2i = mean_Permafrost,
                  sd1i = sd_ActiveLayer, sd2i = sd_Permafrost,
                  n1i = n_ActiveLayer, n2i = n_Permafrost, data = metaA) %>%
  as_tibble() 
head(effects)
```
SMD = 0 ~ There is no difference in CH4 emissions between Active Layer and Permafrost treatments

SMD < 0 ~ Mean of CH4 emissions from Active Layer is lower than mean of CH4 emissions from Permafrost

SMD > 0 ~ Mean of CH4 emissions from Permafrost is lower than mean of CH4 emissions from Active Layer 

```{r AA}
#plot it
forest(effects$yi, effects$vi, slab = paste0(effects$Article, ", ", effects$Temperature, " C"), header=TRUE, top=2)
```

Most effect sizes are positive (excluding Mackelprang  and Treat), suggesting that higher methane emissions are emitted from the active layer compared to the permafrost layer.

```{r AAA}
#fit mixed-effects model with temperature as a moderator, super high p-value, seems time doesn't have a big effect here
res_temp <- rma(yi, vi, mods = ~ Temperature, slab=Article, data=effects)
res_temp
```

There does not appear to be a statistically significant relationship between temperature and effect size.

# CH4 emissions from Active Layer vs Permafrost incubations by time

```{r B}
#load data
ch4_flux_by_layer_time <- read_csv("/Users/lgschaer/Desktop/MetaA_Figs/ch4_flux_by_layer_time.csv")

#wrangle data into necessary format
metaA <- ch4_flux_by_layer_time %>%
  mutate(Layer2 = Layer,
         Layer3 = Layer) %>%
  pivot_wider(names_from = "Layer", values_from = "mean", names_prefix = "mean_") %>%
  pivot_wider(names_from = "Layer2", values_from = "sd", names_prefix = "sd_") %>%
  pivot_wider(names_from = "Layer3", values_from = "n", names_prefix = "n_") %>%
  group_by(Article, Time) %>%
  summarize(
    mean_ActiveLayer = mean(mean_ActiveLayer, na.rm = TRUE),
    n_ActiveLayer = sum(n_ActiveLayer, na.rm = TRUE),
    sd_ActiveLayer = sqrt(sum(((n_ActiveLayer-1)*(sd_ActiveLayer^2)), na.rm = TRUE)/(sum(n_ActiveLayer)-n())),
    mean_Permafrost = mean(mean_Permafrost, na.rm = TRUE),
    n_Permafrost = sum(n_Permafrost, na.rm = TRUE),
    sd_Permafrost = sqrt(sum(((n_Permafrost-1)*(sd_Permafrost^2)), na.rm = TRUE)/(sum(n_Permafrost)-n()))
  )

#use escalc function to calculate effect sizes
##NOTE: I also did this using measure = "SMDH" to account for heteroskedasticity in the data, results were exactly the same
effects <- escalc(measure = "SMD", m1i = mean_ActiveLayer, m2i = mean_Permafrost,
                  sd1i = sd_ActiveLayer, sd2i = sd_Permafrost,
                  n1i = n_ActiveLayer, n2i = n_Permafrost, data = metaA) %>%
  as_tibble() 
head(effects)

```

SMD = 0 ~ There is no difference in CH4 emissions between Active Layer and Permafrost treatments

SMD < 0 ~ Mean of CH4 emissions from Active Layer is lower than mean of CH4 emissions from Permafrost

SMD > 0 ~ Mean of CH4 emissions from Permafrost is lower than mean of CH4 emissions from Active Layer 

```{r BB}
#plot it
forest(effects$yi, effects$vi, slab = paste0(effects$Article, ", ", effects$Time, " days"), header=TRUE, top=2)
```


Again, most effect sizes are positive (excluding Song 2014 et al after 48 days), suggesting that higher methane emissions are emitted from the active layer compared to the permafrost layer.

```{r BBB}
#fit mixed-effects model with temperature as a moderator, super high p-value, seems time doesn't have a big effect here
res <- rma(yi, vi, mods = ~ Time, slab=Article, data=effects)
res
```

There does not appear to be a statistically significant relationship between incubation time and effect size.

###

NOTE: Used the formula here to estimate average standard deviations throughout:

https://www.statology.org/averaging-standard-deviations/
