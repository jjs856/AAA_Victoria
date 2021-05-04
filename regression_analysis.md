---
title: "regression_analysis"
author: "Daniel Fuller"
date: "12/04/2021"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
---



## Libraries


```r
library(lme4)
```

```
## Loading required package: Matrix
```

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x tidyr::expand() masks Matrix::expand()
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
## x tidyr::pack()   masks Matrix::pack()
## x tidyr::unpack() masks Matrix::unpack()
```

```r
library(gtools)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(DRDID)
library(did)
```

```
## Registered S3 methods overwritten by 'car':
##   method                          from
##   influence.merMod                lme4
##   cooks.distance.influence.merMod lme4
##   dfbeta.influence.merMod         lme4
##   dfbetas.influence.merMod        lme4
```

```r
library(modelsummary)
library(broom.mixed)
```

```
## Registered S3 method overwritten by 'broom.mixed':
##   method      from 
##   tidy.gamlss broom
```


## Analysis plan

1. Descriptive statistics outcome and exposure
2. Merge in demographic and weather variables 
3. Regression outcome and exposure (as per change discussions)
4. Sensitivity analysis (exposure changes)

### Read in data


```r
data <- read_csv("/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   utcdate = col_datetime(format = ""),
##   zone = col_character(),
##   activity_levels = col_character(),
##   gender = col_character(),
##   income = col_character(),
##   education = col_logical(),
##   city_id = col_character(),
##   date = col_date(format = "")
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning: 550640 parsing failures.
##    row       col           expected            actual                                                                  file
## 708730 education 1/0/T/F/TRUE/FALSE University degree '/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv'
## 708731 education 1/0/T/F/TRUE/FALSE University degree '/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv'
## 708732 education 1/0/T/F/TRUE/FALSE University degree '/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv'
## 708733 education 1/0/T/F/TRUE/FALSE University degree '/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv'
## 708734 education 1/0/T/F/TRUE/FALSE University degree '/Users/dfuller/Documents/INTERACT/data/sensors/sd_data_exposure.csv'
## ...... ......... .................. ................. .....................................................................
## See problems(...) for more details.
```

## Demographic data and weather

### Merging demographics


```r
health_data <- read_csv("/Users/dfuller/Documents/INTERACT/data/health_clean.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   city_id = col_character(),
##   questionnaire_lang.x = col_character(),
##   date_of_survey = col_date(format = ""),
##   mode_used = col_character(),
##   mode_used_txt = col_character(),
##   cars_access_outside = col_character(),
##   cars_access_outside_txt = col_character(),
##   bike_access_options = col_character(),
##   bike_access_options_txt = col_character(),
##   tracking1_txt = col_character(),
##   house_tenure_txt = col_character(),
##   dwelling_type_txt = col_character(),
##   residence = col_date(format = ""),
##   gender_txt = col_character(),
##   sex_txt = col_logical(),
##   living_arrange = col_character(),
##   living_arrange_txt = col_character(),
##   group_id_mtl = col_character(),
##   group_id_mtl_txt = col_character(),
##   employment_txt = col_character()
##   # ... with 204 more columns
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning: 35032 parsing failures.
##  row            col           expected actual                                                      file
## 1156 sask_bus_pass  1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_safe       1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_reliable   1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_convenient 1/0/T/F/TRUE/FALSE     2  '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## 1156 bus_freq_a     1/0/T/F/TRUE/FALSE     91 '/Users/dfuller/Documents/INTERACT/data/health_clean.csv'
## .... .............. .................. ...... .........................................................
## See problems(...) for more details.
```

```r
health_data <- health_data %>%
                  select(education_recode, gender_recode2, income_recode, age_recode, white, asian, indigenous, black, latin_american, 
                         middle_eastern, other_ethnicity, interact_id)

data <- left_join(data, health_data, by = c("interact_id"))
```

### Merging weather


```r
weather_data <- read_csv("/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   lon = col_double(),
##   lat = col_double(),
##   climate_id = col_double(),
##   date_time = col_date(format = ""),
##   year = col_double(),
##   data_quality = col_logical(),
##   max_temp_c = col_double(),
##   min_temp_c = col_double(),
##   mean_temp_c = col_double(),
##   heat_days_c = col_double(),
##   cool_days_c = col_double(),
##   total_rain_mm = col_double(),
##   total_snow_mm = col_double(),
##   total_precip_mm = col_double(),
##   snow_ground_cm = col_double(),
##   snow_ground_flag = col_logical(),
##   dir_gust_10s_deg = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

```
## Warning: 1835 parsing failures.
##  row              col           expected actual                                                                                    file
## 1135 snow_ground_flag 1/0/T/F/TRUE/FALSE      E '/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv'
## 1473 snow_ground_flag 1/0/T/F/TRUE/FALSE      E '/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv'
## 1817 snow_ground_flag 1/0/T/F/TRUE/FALSE      E '/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv'
## 2964 snow_ground_flag 1/0/T/F/TRUE/FALSE      E '/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv'
## 2968 snow_ground_flag 1/0/T/F/TRUE/FALSE      E '/Users/dfuller/Documents/INTERACT/data/weather/2016_2020_weather_vic_van_sask_mtl.csv'
## .... ................ .................. ...... .......................................................................................
## See problems(...) for more details.
```

```r
weather_data <- weather_data %>% dplyr::select(date_time, city_id, max_temp_c, min_temp_c, mean_temp_c, total_rain_mm, total_snow_mm, total_precip_mm, snow_ground_cm, speed_gust_km_h)

weather_data$date <- ymd(weather_data$date_time)

weather_data <- weather_data %>% filter(city_id == "Victoria")

weather_data <- arrange(weather_data, date)
data <- arrange(data, date)

data <- left_join(data, weather_data, by = c("date"))
```

### Exposure over time 


```r
## Overall 
table(data$wave_id, data$exposed_70)
```

```
##    
##          0      1
##   1 690274  18455
##   2 519463  35876
```

```r
## Individual level
data_exposure <- data %>%
                  group_by(interact_id, wave_id, date) %>%
                    summarise(
                      sum_exp_70 = sum(exposed_70), 
                      sum_exp_50 = sum(exposed_50), 
                      sum_exp_100 = sum(exposed_100), 
                      mvpa = first(total_mvpa_minutes),
                      sed = first(total_sed_minutes),
                      light = first(total_light_pa_minutes),
                      rel_exp_70 = sum(exposed_70)/n(),
                      rel_exp_50 = sum(exposed_50)/n(),
                      rel_exp_100 = sum(exposed_100)/n(),
                      minutes = first(minutes_id_date),
                      mean_temp_c = first(mean_temp_c),
                      total_precip_mm = first(total_precip_mm),
                      snow_ground_cm = first(snow_ground_cm),
                      speed_gust_km_h = first(speed_gust_km_h),
                      education_recode = first(education_recode),
                      gender_recode2 = first(gender_recode2),
                      income_recode = first(income_recode),
                      age_recode = first(age_recode),
                      white = first(white),
                      asian = first(asian),
                      indigenous = first(indigenous),
                      black = first(black),
                      latin_american = first(latin_american),
                      middle_eastern = first(middle_eastern),
                      other_ethnicity - first(other_ethnicity)
                    )
```

```
## `summarise()` regrouping output by 'interact_id', 'wave_id', 'date' (override with `.groups` argument)
```

```r
data_exposure$date_time <- as.factor(data_exposure$date)

data_exposure$speed_gust_km_h <- as.numeric(data_exposure$speed_gust_km_h)
```

```
## Warning: NAs introduced by coercion
```

### Histograms of absolute exposure (Total number of minutes)


```r
summary(data_exposure$sum_exp_70)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00    0.00    2.00   24.37   13.00  652.00
```

```r
### Removing participants with more than 500 minutes of exposure
data_exposure$sum_exp_70 <- if_else(data_exposure$sum_exp_70 > 500, NA_real_, data_exposure$sum_exp_70)
data_exposure$sum_exp_50 <- if_else(data_exposure$sum_exp_50 > 500, NA_real_, data_exposure$sum_exp_50)
data_exposure$sum_exp_100 <- if_else(data_exposure$sum_exp_100 > 500, NA_real_, data_exposure$sum_exp_100)

data_exposure <- data_exposure %>% 
                    mutate(
                      sum_exp_70_quint = ntile(sum_exp_70, 5),
                      sum_exp_50_quint = ntile(sum_exp_50, 5),
                      sum_exp_100_quint = ntile(sum_exp_100, 5)
                      )

data_exposure$sum_exp_70_quint <- as.factor(data_exposure$sum_exp_70_quint)
data_exposure$sum_exp_50_quint <- as.factor(data_exposure$sum_exp_50_quint)
data_exposure$sum_exp_100_quint <- as.factor(data_exposure$sum_exp_100_quint)

table(data_exposure$sum_exp_70_quint)
```

```
## 
##      1      2      3      4      5 
## 252741 252150 251586 251090 250472
```

```r
table(data_exposure$sum_exp_100_quint)
```

```
## 
##      1      2      3      4      5 
## 252227 251638 251075 250579 249961
```


```r
abs_exposure_histo <- ggplot(data_exposure, aes(sum_exp_70)) + 
  geom_histogram() + 
  facet_wrap(~ wave_id) + 
  theme_classic()

plot(abs_exposure_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 6029 rows containing non-finite values (stat_bin).
```

![](regression_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### Histograms of relative exposure (% of total minutes exposed)


```r
summary(data_exposure$rel_exp_70)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000000 0.000000 0.004057 0.042981 0.023684 1.000000
```

```r
### Removing participants with more than 40% exposure
data_exposure$rel_exp_70 <- if_else(data_exposure$rel_exp_70 > 0.40, NA_real_, data_exposure$rel_exp_70)
data_exposure$rel_exp_50 <- if_else(data_exposure$rel_exp_50 > 0.40, NA_real_, data_exposure$rel_exp_50)
data_exposure$rel_exp_100 <- if_else(data_exposure$rel_exp_100 > 0.40, NA_real_, data_exposure$rel_exp_100)

data_exposure <- data_exposure %>% 
                    mutate(
                      rel_exp_70_quint = ntile(rel_exp_70, 5),
                      rel_exp_50_quint = ntile(rel_exp_50, 5),
                      rel_exp_100_quint = ntile(rel_exp_100, 5)
                      )
data_exposure$rel_exp_70_quint <- as.factor(data_exposure$rel_exp_70_quint)
data_exposure$rel_exp_50_quint <- as.factor(data_exposure$rel_exp_50_quint)
data_exposure$rel_exp_100_quint <- as.factor(data_exposure$rel_exp_100_quint)

table(data_exposure$rel_exp_70_quint)
```

```
## 
##      1      2      3      4      5 
## 245996 245424 244873 244391 243790
```

```r
table(data_exposure$rel_exp_100_quint)
```

```
## 
##      1      2      3      4      5 
## 244839 244273 243726 243248 242649
```


```r
rel_exposure_histo <- ggplot(data_exposure, aes(rel_exp_70)) + 
  geom_histogram() + 
  facet_wrap(~ wave_id) + 
  theme_classic()

plot(rel_exposure_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 39594 rows containing non-finite values (stat_bin).
```

![](regression_analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Histograms of Moderate to vigorous PA 


```r
pa_histo <- ggplot(data_exposure, aes(mvpa)) + 
  geom_histogram() + 
  facet_wrap(~ wave_id) + 
  theme_classic()

plot(pa_histo)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](regression_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Scatterplot Absolute Exposure and Moderate to vigorous PA 


```r
abs_exp_pa_scatter <- ggplot(data_exposure, aes(x = sum_exp_70, y = mvpa)) + 
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ wave_id) + 
  theme_classic()

plot(abs_exp_pa_scatter)
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

```
## Warning: Removed 6029 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 6029 rows containing missing values (geom_point).
```

![](regression_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Scatterplot Relative Exposure and Moderate to vigorous PA 


```r
rel_exp_pa_scatter <- ggplot(data_exposure, aes(x = rel_exp_70, y = mvpa)) + 
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ wave_id) + 
  theme_classic()

plot(rel_exp_pa_scatter)
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

```
## Warning: Removed 39594 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 39594 rows containing missing values (geom_point).
```

![](regression_analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### Linear regression


```r
lm_models <- list(
  "Absolute Exposure" = lm(mvpa ~ sum_exp_70*factor(wave_id), data = data_exposure),
  "Relative Exposure" = lm(mvpa ~ rel_exp_70*factor(wave_id), data = data_exposure)
)

modelsummary(lm_models, fmt = "%.2f", statistic = 'conf.int')
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Absolute Exposure </th>
   <th style="text-align:center;"> Relative Exposure </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:center;"> 33.28 </td>
   <td style="text-align:center;"> 33.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [33.19, 33.36] </td>
   <td style="text-align:center;"> [33.49, 33.66] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sum_exp_70 </td>
   <td style="text-align:center;"> 0.13 </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [0.13, 0.13] </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> factor(wave_id)2 </td>
   <td style="text-align:center;"> 2.63 </td>
   <td style="text-align:center;"> 1.97 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [2.50, 2.76] </td>
   <td style="text-align:center;"> [1.83, 2.10] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sum_exp_70 × factor(wave_id)2 </td>
   <td style="text-align:center;"> -0.09 </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> [-0.09, -0.09] </td>
   <td style="text-align:center;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rel_exp_70 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> 58.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> [57.40, 60.47] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rel_exp_70 × factor(wave_id)2 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> -20.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> [-22.97, -18.52] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 1258039 </td>
   <td style="text-align:center;"> 1224474 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:center;"> 0.021 </td>
   <td style="text-align:center;"> 0.007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 Adj. </td>
   <td style="text-align:center;"> 0.021 </td>
   <td style="text-align:center;"> 0.007 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AIC </td>
   <td style="text-align:center;"> 12474739.8 </td>
   <td style="text-align:center;"> 12132552.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BIC </td>
   <td style="text-align:center;"> 12474800.0 </td>
   <td style="text-align:center;"> 12132612.7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Log.Lik. </td>
   <td style="text-align:center;"> -6237364.880 </td>
   <td style="text-align:center;"> -6066271.299 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> F </td>
   <td style="text-align:center;"> 8964.212 </td>
   <td style="text-align:center;"> 2982.632 </td>
  </tr>
</tbody>
</table>

## Multilevel models 

MLM models with person level random intercepts using the lmer package

### Absolute exposure with a 70 meter buffer and no covariates


```r
### Null Model
lmer_null_exp70_ln <- lmer(mvpa ~ 1 + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_null_exp70_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ 1 + (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11793864
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5956 -0.6201 -0.0680  0.4895  5.3456 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 387.4    19.68   
##  interact_id (Intercept) 601.8    24.53   
##  Residual                657.8    25.65   
## Number of obs: 1264068, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   31.268      1.998   15.65
```

```r
broom.mixed::glance(lmer_null_exp70_ln)
```

```
## # A tibble: 1 x 6
##   sigma    logLik       AIC       BIC  REMLcrit df.residual
##   <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <int>
## 1  25.6 -5896932. 11793872. 11793920. 11793864.     1264064
```


```r
### Absolute Model Continuous
lmer_abs_exp70_ln <- lmer(mvpa ~ sum_exp_70*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp70_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_70 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11693416
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6239 -0.6205 -0.0685  0.4924  5.3155 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 368.4    19.19   
##  interact_id (Intercept) 578.7    24.06   
##  Residual                635.0    25.20   
## Number of obs: 1258039, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                              Estimate Std. Error t value
## (Intercept)                 34.060747   2.193749  15.526
## sum_exp_70                   0.112015   0.001121  99.880
## factor(wave_id)2            -9.383958   2.080260  -4.511
## sum_exp_70:factor(wave_id)2 -0.067613   0.001295 -52.191
## 
## Correlation of Fixed Effects:
##             (Intr) sm__70 fc(_)2
## sum_exp_70  -0.007              
## fctr(wv_d)2 -0.452  0.004       
## sm__70:(_)2  0.005 -0.854 -0.007
```

```r
broom.mixed::glance(lmer_abs_exp70_ln)
```

```
## # A tibble: 1 x 6
##   sigma    logLik       AIC       BIC  REMLcrit df.residual
##   <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <int>
## 1  25.2 -5846708. 11693430. 11693514. 11693416.     1258032
```

```r
### Absolute Model Quintiles
lmer_abs_exp70_q <- lmer(mvpa ~ sum_exp_70_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp70_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_70_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11707345
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6649 -0.6214 -0.0692  0.4893  5.2771 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 365.5    19.12   
##  interact_id (Intercept) 598.7    24.47   
##  Residual                642.1    25.34   
## Number of obs: 1258039, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                      Estimate Std. Error t value
## (Intercept)                        35.7553267  2.2124153  16.161
## sum_exp_70_quint2                   0.0116068  0.0950237   0.122
## sum_exp_70_quint3                   0.0196437  0.0950777   0.207
## sum_exp_70_quint4                   0.0301662  0.0951244   0.317
## sum_exp_70_quint5                   0.0396170  0.0951846   0.416
## factor(wave_id)2                   -9.3715729  2.0740308  -4.519
## sum_exp_70_quint2:factor(wave_id)2 -0.0001054  0.1438100  -0.001
## sum_exp_70_quint3:factor(wave_id)2  0.0039051  0.1438904   0.027
## sum_exp_70_quint4:factor(wave_id)2  0.0040488  0.1439619   0.028
## sum_exp_70_quint5:factor(wave_id)2  0.0007502  0.1440504   0.005
## 
## Correlation of Fixed Effects:
##             (Intr) sm__70_2 sm__70_3 sm__70_4 sm__70_5 fc(_)2 s__70_2: s__70_3:
## sm_xp_70_q2 -0.021                                                             
## sm_xp_70_q3 -0.021  0.499                                                      
## sm_xp_70_q4 -0.021  0.499    0.499                                             
## sm_xp_70_q5 -0.021  0.499    0.498    0.498                                    
## fctr(wv_d)2 -0.447  0.023    0.023    0.023    0.023                           
## s__70_2:(_)  0.014 -0.661   -0.330   -0.330   -0.329   -0.035                  
## s__70_3:(_)  0.014 -0.330   -0.661   -0.329   -0.329   -0.035  0.499           
## s__70_4:(_)  0.014 -0.330   -0.329   -0.661   -0.329   -0.035  0.499    0.499  
## s__70_5:(_)  0.014 -0.329   -0.329   -0.329   -0.661   -0.035  0.499    0.498  
##             s__70_4:
## sm_xp_70_q2         
## sm_xp_70_q3         
## sm_xp_70_q4         
## sm_xp_70_q5         
## fctr(wv_d)2         
## s__70_2:(_)         
## s__70_3:(_)         
## s__70_4:(_)         
## s__70_5:(_)  0.498
```

```r
broom.mixed::glance(lmer_abs_exp70_q)
```

```
## # A tibble: 1 x 6
##   sigma    logLik       AIC       BIC  REMLcrit df.residual
##   <dbl>     <dbl>     <dbl>     <dbl>     <dbl>       <int>
## 1  25.3 -5853672. 11707371. 11707527. 11707345.     1258026
```

### Absolute exposure with a 70 meter buffer and covariates

```r
### Absolute Model Continuous
lmer_abs_exp70_ln_cov <- lmer(mvpa ~ sum_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp70_ln_cov)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_70 * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 6453454
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9465 -0.5699 -0.0554  0.5054  5.3845 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 326.9    18.08   
##  interact_id (Intercept) 494.3    22.23   
##  Residual                552.4    23.50   
## Number of obs: 704845, groups:  date_time, 225; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                               31.505698  12.321525   2.557
## sum_exp_70                                 0.115439   0.001296  89.041
## factor(wave_id)2                           0.932909   3.185039   0.293
## mean_temp_c                               -0.544184   0.301100  -1.807
## total_precip_mm                           -0.013153   0.255321  -0.052
## speed_gust_km_h                            0.070658   0.136199   0.519
## gender_recode2Transgender                -22.910528  16.215112  -1.413
## gender_recode2Woman                       -9.002616   4.034274  -2.232
## income_recode100_200                      -0.043974  11.056868  -0.004
## income_recode20_49 999                    12.140214  11.492475   1.056
## income_recode200+                         19.114408  24.883528   0.768
## income_recode50_99 999                    -0.448353  10.735565  -0.042
## income_recodeDon't know/prefer no answer   4.633075  12.896138   0.359
## age_recode30_39                            7.741094   6.329654   1.223
## age_recode40_49                            9.105662   6.714743   1.356
## age_recode50_64                            8.883314   7.574856   1.173
## age_recode65+                             -0.334705   7.983239  -0.042
## sum_exp_70:factor(wave_id)2               -0.157591   0.001728 -91.194
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
broom.mixed::glance(lmer_abs_exp70_ln_cov)
```

```
## # A tibble: 1 x 6
##   sigma    logLik      AIC      BIC REMLcrit df.residual
##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
## 1  23.5 -3226727. 6453496. 6453736. 6453454.      704824
```

```r
### Absolute Model Quintiles
lmer_abs_exp70_q_cov <- lmer(mvpa ~ sum_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp70_q_cov)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## mvpa ~ sum_exp_70_quint * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 6462817
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9786 -0.5564 -0.0447  0.4969  5.3555 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 324.5    18.01   
##  interact_id (Intercept) 474.0    21.77   
##  Residual                559.8    23.66   
## Number of obs: 704845, groups:  date_time, 225; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                               32.230501  12.128282   2.657
## sum_exp_70_quint2                          0.010197   0.097314   0.105
## sum_exp_70_quint3                          0.018344   0.097369   0.188
## sum_exp_70_quint4                          0.029048   0.097419   0.298
## sum_exp_70_quint5                          0.036122   0.097481   0.371
## factor(wave_id)2                          -1.700241   3.176737  -0.535
## mean_temp_c                               -0.505224   0.300001  -1.684
## total_precip_mm                           -0.017584   0.254380  -0.069
## speed_gust_km_h                            0.047873   0.135696   0.353
## gender_recode2Transgender                -22.086227  15.878532  -1.391
## gender_recode2Woman                       -8.393268   3.950562  -2.125
## income_recode100_200                      -0.480690  10.827360  -0.044
## income_recode20_49 999                    12.874443  11.253911   1.144
## income_recode200+                         17.045033  24.367059   0.700
## income_recode50_99 999                     0.262062  10.512710   0.025
## income_recodeDon't know/prefer no answer   4.264155  12.628457   0.338
## age_recode30_39                            8.184408   6.198305   1.320
## age_recode40_49                            9.797109   6.575328   1.490
## age_recode50_64                            8.758135   7.417635   1.181
## age_recode65+                             -0.606259   7.817576  -0.078
## sum_exp_70_quint2:factor(wave_id)2         0.003353   0.240179   0.014
## sum_exp_70_quint3:factor(wave_id)2         0.007679   0.240338   0.032
## sum_exp_70_quint4:factor(wave_id)2        -0.003539   0.240456  -0.015
## sum_exp_70_quint5:factor(wave_id)2        -0.001609   0.240573  -0.007
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
broom.mixed::glance(lmer_abs_exp70_q_cov)
```

```
## # A tibble: 1 x 6
##   sigma    logLik      AIC      BIC REMLcrit df.residual
##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
## 1  23.7 -3231409. 6462871. 6463181. 6462817.      704818
```

## Relative Exposure

### Relative exposure with a 70 meter buffer and no covariates


```r
### Linear exposure
lmer_rel_exp_ln <- lmer(mvpa ~ rel_exp_70*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_70 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11380632
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5996 -0.6125 -0.0567  0.5016  5.3120 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 398.7    19.97   
##  interact_id (Intercept) 605.4    24.60   
##  Residual                634.6    25.19   
## Number of obs: 1224474, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                  34.4700     2.2601  15.252
## rel_exp_70                   40.1005     0.8087  49.585
## factor(wave_id)2            -11.6300     2.1648  -5.372
## rel_exp_70:factor(wave_id)2  28.9485     1.1075  26.137
## 
## Correlation of Fixed Effects:
##             (Intr) rl__70 fc(_)2
## rel_exp_70  -0.008              
## fctr(wv_d)2 -0.456  0.005       
## rl__70:(_)2  0.004 -0.710 -0.010
```

```r
### Quintiles of exposure
lmer_rel_exp_q <- lmer(mvpa ~ rel_exp_70_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_70_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11390678
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6732 -0.6171 -0.0682  0.4862  5.2779 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 395.6    19.89   
##  interact_id (Intercept) 614.7    24.79   
##  Residual                639.8    25.29   
## Number of obs: 1224474, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                      Estimate Std. Error t value
## (Intercept)                         3.570e+01  2.267e+00  15.749
## rel_exp_70_quint2                   1.118e-02  9.548e-02   0.117
## rel_exp_70_quint3                   1.911e-02  9.554e-02   0.200
## rel_exp_70_quint4                   2.926e-02  9.558e-02   0.306
## rel_exp_70_quint5                   3.882e-02  9.564e-02   0.406
## factor(wave_id)2                   -1.032e+01  2.158e+00  -4.781
## rel_exp_70_quint2:factor(wave_id)2  1.163e-03  1.458e-01   0.008
## rel_exp_70_quint3:factor(wave_id)2  6.265e-03  1.459e-01   0.043
## rel_exp_70_quint4:factor(wave_id)2  4.671e-03  1.460e-01   0.032
## rel_exp_70_quint5:factor(wave_id)2  9.524e-04  1.461e-01   0.007
## 
## Correlation of Fixed Effects:
##             (Intr) rl__70_2 rl__70_3 rl__70_4 rl__70_5 fc(_)2 r__70_2: r__70_3:
## rl_xp_70_q2 -0.021                                                             
## rl_xp_70_q3 -0.021  0.499                                                      
## rl_xp_70_q4 -0.021  0.499    0.499                                             
## rl_xp_70_q5 -0.021  0.499    0.498    0.498                                    
## fctr(wv_d)2 -0.454  0.022    0.022    0.022    0.022                           
## r__70_2:(_)  0.014 -0.655   -0.327   -0.327   -0.326   -0.034                  
## r__70_3:(_)  0.014 -0.327   -0.655   -0.326   -0.326   -0.034  0.499           
## r__70_4:(_)  0.014 -0.327   -0.326   -0.655   -0.326   -0.034  0.499    0.499  
## r__70_5:(_)  0.014 -0.326   -0.326   -0.326   -0.655   -0.034  0.499    0.498  
##             r__70_4:
## rl_xp_70_q2         
## rl_xp_70_q3         
## rl_xp_70_q4         
## rl_xp_70_q5         
## fctr(wv_d)2         
## r__70_2:(_)         
## r__70_3:(_)         
## r__70_4:(_)         
## r__70_5:(_)  0.498
```

### Relative exposure with a 70 meter buffer and covariates


```r
### Linear exposure
lmer_rel_exp_ln_cov <- lmer(mvpa ~ rel_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp_ln_cov)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_70 * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 6332230
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8824 -0.5607 -0.0441  0.5078  5.3225 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 332.3    18.23   
##  interact_id (Intercept) 517.7    22.75   
##  Residual                557.9    23.62   
## Number of obs: 690856, groups:  date_time, 222; interact_id, 134
## 
## Fixed effects:
##                                           Estimate Std. Error t value
## (Intercept)                               32.41905   12.56427   2.580
## rel_exp_70                                46.08715    0.85719  53.766
## factor(wave_id)2                           1.06485    3.27574   0.325
## mean_temp_c                               -0.65756    0.30475  -2.158
## total_precip_mm                            0.02447    0.25788   0.095
## speed_gust_km_h                            0.09775    0.13812   0.708
## gender_recode2Transgender                -23.73921   16.59467  -1.431
## gender_recode2Woman                       -9.98663    4.12891  -2.419
## income_recode100_200                      -0.96709   11.31565  -0.085
## income_recode20_49 999                    12.75920   11.76139   1.085
## income_recode200+                         19.62992   25.46574   0.771
## income_recode50_99 999                    -0.27729   10.98682  -0.025
## income_recodeDon't know/prefer no answer   4.21482   13.19789   0.319
## age_recode30_39                            8.07024    6.47782   1.246
## age_recode40_49                           11.10223    6.87251   1.615
## age_recode50_64                            9.75671    7.75218   1.259
## age_recode65+                             -0.55047    8.17008  -0.067
## rel_exp_70:factor(wave_id)2              -35.29943    1.66885 -21.152
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
### Quintiles of exposure
lmer_rel_exp_q_cov <- lmer(mvpa ~ rel_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp_q_cov)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## mvpa ~ rel_exp_70_quint * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 6335156
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.9704 -0.5600 -0.0423  0.4976  5.3464 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 324.1    18.00   
##  interact_id (Intercept) 519.5    22.79   
##  Residual                560.3    23.67   
## Number of obs: 690856, groups:  date_time, 222; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                               32.053453  12.535069   2.557
## rel_exp_70_quint2                          0.010354   0.097921   0.106
## rel_exp_70_quint3                          0.017947   0.097976   0.183
## rel_exp_70_quint4                          0.028390   0.098026   0.290
## rel_exp_70_quint5                          0.035381   0.098088   0.361
## factor(wave_id)2                           0.263504   3.238456   0.081
## mean_temp_c                               -0.579268   0.300995  -1.925
## total_precip_mm                            0.004761   0.254678   0.019
## speed_gust_km_h                            0.092489   0.136406   0.678
## gender_recode2Transgender                -23.363715  16.623430  -1.405
## gender_recode2Woman                       -9.238491   4.136034  -2.234
## income_recode100_200                      -0.526550  11.335289  -0.046
## income_recode20_49 999                    12.895443  11.781802   1.095
## income_recode200+                         17.638534  25.509919   0.691
## income_recode50_99 999                     0.138276  11.005885   0.013
## income_recodeDon't know/prefer no answer   3.898057  13.220799   0.295
## age_recode30_39                            8.113320   6.489050   1.250
## age_recode40_49                           10.703042   6.884436   1.555
## age_recode50_64                            8.939917   7.765621   1.151
## age_recode65+                             -0.939608   8.184225  -0.115
## rel_exp_70_quint2:factor(wave_id)2         0.004989   0.247074   0.020
## rel_exp_70_quint3:factor(wave_id)2         0.012424   0.247235   0.050
## rel_exp_70_quint4:factor(wave_id)2         0.001892   0.247359   0.008
## rel_exp_70_quint5:factor(wave_id)2         0.005227   0.247480   0.021
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

# Sensitivity Analysis for different buffers

## 50 Meter Buffer

Absolute Exposure 


```r
### Absolute Model Continuous
lmer_abs_exp50_ln <- lmer(mvpa ~ sum_exp_50*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp50_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_50 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11777746
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5882 -0.6102 -0.0693  0.4968  5.1566 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 368.5    19.20   
##  interact_id (Intercept) 587.1    24.23   
##  Residual                652.6    25.55   
## Number of obs: 1263404, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                              Estimate Std. Error t value
## (Intercept)                 34.308052   2.203054  15.573
## sum_exp_50                   0.117191   0.001324  88.519
## factor(wave_id)2            -9.219630   2.080847  -4.431
## sum_exp_50:factor(wave_id)2 -0.079950   0.001494 -53.529
## 
## Correlation of Fixed Effects:
##             (Intr) sm__50 fc(_)2
## sum_exp_50  -0.007              
## fctr(wv_d)2 -0.450  0.005       
## sm__50:(_)2  0.005 -0.882 -0.007
```

```r
### Absolute Model Quintiles
lmer_abs_exp50_q <- lmer(mvpa ~ sum_exp_50_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp50_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_50_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11788239
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5950 -0.6201 -0.0680  0.4896  5.3444 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 368.3    19.19   
##  interact_id (Intercept) 603.0    24.56   
##  Residual                658.1    25.65   
## Number of obs: 1263404, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                      Estimate Std. Error t value
## (Intercept)                         3.576e+01  2.221e+00  16.104
## sum_exp_50_quint2                   1.158e-02  9.620e-02   0.120
## sum_exp_50_quint3                   1.962e-02  9.625e-02   0.204
## sum_exp_50_quint4                   3.010e-02  9.630e-02   0.313
## sum_exp_50_quint5                   3.953e-02  9.636e-02   0.410
## factor(wave_id)2                   -9.442e+00  2.082e+00  -4.535
## sum_exp_50_quint2:factor(wave_id)2  6.095e-04  1.452e-01   0.004
## sum_exp_50_quint3:factor(wave_id)2  3.586e-03  1.453e-01   0.025
## sum_exp_50_quint4:factor(wave_id)2  3.099e-03  1.453e-01   0.021
## sum_exp_50_quint5:factor(wave_id)2  5.792e-05  1.454e-01   0.000
## 
## Correlation of Fixed Effects:
##             (Intr) sm__50_2 sm__50_3 sm__50_4 sm__50_5 fc(_)2 s__50_2: s__50_3:
## sm_xp_50_q2 -0.022                                                             
## sm_xp_50_q3 -0.022  0.499                                                      
## sm_xp_50_q4 -0.022  0.499    0.499                                             
## sm_xp_50_q5 -0.022  0.499    0.498    0.498                                    
## fctr(wv_d)2 -0.447  0.023    0.023    0.023    0.023                           
## s__50_2:(_)  0.014 -0.663   -0.331   -0.331   -0.330   -0.035                  
## s__50_3:(_)  0.014 -0.331   -0.663   -0.330   -0.330   -0.035  0.499           
## s__50_4:(_)  0.014 -0.331   -0.330   -0.663   -0.330   -0.035  0.499    0.499  
## s__50_5:(_)  0.014 -0.330   -0.330   -0.330   -0.663   -0.035  0.499    0.498  
##             s__50_4:
## sm_xp_50_q2         
## sm_xp_50_q3         
## sm_xp_50_q4         
## sm_xp_50_q5         
## fctr(wv_d)2         
## s__50_2:(_)         
## s__50_3:(_)         
## s__50_4:(_)         
## s__50_5:(_)  0.498
```

### Relative Exposure


```r
### Linear exposure
lmer_rel_exp50_ln <- lmer(mvpa ~ rel_exp_50*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp50_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_50 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11498291
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5642 -0.6128 -0.0560  0.4886  5.2598 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 391.6    19.79   
##  interact_id (Intercept) 605.4    24.60   
##  Residual                647.5    25.45   
## Number of obs: 1234470, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                  34.4388     2.2514  15.296
## rel_exp_50                   48.2402     0.9188  52.501
## factor(wave_id)2            -11.2397     2.1448  -5.241
## rel_exp_50:factor(wave_id)2  27.2436     1.1994  22.714
## 
## Correlation of Fixed Effects:
##             (Intr) rl__50 fc(_)2
## rel_exp_50  -0.008              
## fctr(wv_d)2 -0.454  0.005       
## rl__50:(_)2  0.004 -0.751 -0.009
```

```r
### Quintiles of exposure
lmer_rel_exp50_q <- lmer(mvpa ~ rel_exp_50_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp50_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_50_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11509895
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6122 -0.6108 -0.0689  0.4826  5.2192 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 391.3    19.78   
##  interact_id (Intercept) 623.5    24.97   
##  Residual                653.6    25.56   
## Number of obs: 1234470, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                      Estimate Std. Error t value
## (Intercept)                         3.577e+01  2.271e+00  15.749
## rel_exp_50_quint2                   1.095e-02  9.632e-02   0.114
## rel_exp_50_quint3                   1.881e-02  9.638e-02   0.195
## rel_exp_50_quint4                   2.904e-02  9.642e-02   0.301
## rel_exp_50_quint5                   3.870e-02  9.648e-02   0.401
## factor(wave_id)2                   -1.007e+01  2.146e+00  -4.691
## rel_exp_50_quint2:factor(wave_id)2  1.205e-03  1.467e-01   0.008
## rel_exp_50_quint3:factor(wave_id)2  4.705e-03  1.468e-01   0.032
## rel_exp_50_quint4:factor(wave_id)2  3.969e-03  1.468e-01   0.027
## rel_exp_50_quint5:factor(wave_id)2  7.285e-04  1.469e-01   0.005
## 
## Correlation of Fixed Effects:
##             (Intr) rl__50_2 rl__50_3 rl__50_4 rl__50_5 fc(_)2 r__50_2: r__50_3:
## rl_xp_50_q2 -0.021                                                             
## rl_xp_50_q3 -0.021  0.499                                                      
## rl_xp_50_q4 -0.021  0.499    0.499                                             
## rl_xp_50_q5 -0.021  0.499    0.498    0.498                                    
## fctr(wv_d)2 -0.451  0.022    0.022    0.022    0.022                           
## r__50_2:(_)  0.014 -0.657   -0.328   -0.328   -0.327   -0.034                  
## r__50_3:(_)  0.014 -0.328   -0.657   -0.327   -0.327   -0.034  0.499           
## r__50_4:(_)  0.014 -0.328   -0.327   -0.657   -0.327   -0.034  0.499    0.499  
## r__50_5:(_)  0.014 -0.327   -0.327   -0.327   -0.657   -0.034  0.499    0.498  
##             r__50_4:
## rl_xp_50_q2         
## rl_xp_50_q3         
## rl_xp_50_q4         
## rl_xp_50_q5         
## fctr(wv_d)2         
## r__50_2:(_)         
## r__50_3:(_)         
## r__50_4:(_)         
## r__50_5:(_)  0.498
```

## 100 Meter Buffer

Absolute Exposure 


```r
### Absolute Model Continuous
lmer_abs_exp100_ln <- lmer(mvpa ~ sum_exp_100*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
## Model failed to converge with max|grad| = 0.00220945 (tol = 0.002, component 1)
```

```r
summary(lmer_abs_exp100_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_100 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11671942
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5955 -0.6236 -0.0703  0.4939  5.3068 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 368.4    19.19   
##  interact_id (Intercept) 585.6    24.20   
##  Residual                636.2    25.22   
## Number of obs: 1255480, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                               Estimate Std. Error t value
## (Intercept)                  34.110666   2.201211  15.496
## sum_exp_100                   0.083612   0.000911  91.778
## factor(wave_id)2             -9.491364   2.080395  -4.562
## sum_exp_100:factor(wave_id)2 -0.044812   0.001111 -40.341
## 
## Correlation of Fixed Effects:
##             (Intr) sm__100 fc(_)2
## sum_exp_100 -0.007               
## fctr(wv_d)2 -0.451  0.004        
## s__100:(_)2  0.004 -0.798  -0.008
## optimizer (nloptwrap) convergence code: 0 (OK)
## Model failed to converge with max|grad| = 0.00220945 (tol = 0.002, component 1)
```

```r
### Absolute Model Quintiles
lmer_abs_exp100_q <- lmer(mvpa ~ sum_exp_100_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_abs_exp100_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_100_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11683306
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6657 -0.6245 -0.0720  0.4870  5.2761 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 364.5    19.09   
##  interact_id (Intercept) 599.9    24.49   
##  Residual                642.0    25.34   
## Number of obs: 1255480, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                       Estimate Std. Error t value
## (Intercept)                         35.6183406  2.2124998  16.099
## sum_exp_100_quint2                   0.0115503  0.0950738   0.121
## sum_exp_100_quint3                   0.0196742  0.0951278   0.207
## sum_exp_100_quint4                   0.0302001  0.0951745   0.317
## sum_exp_100_quint5                   0.0395910  0.0952348   0.416
## factor(wave_id)2                    -9.2606338  2.0712184  -4.471
## sum_exp_100_quint2:factor(wave_id)2  0.0004275  0.1439598   0.003
## sum_exp_100_quint3:factor(wave_id)2  0.0042101  0.1440404   0.029
## sum_exp_100_quint4:factor(wave_id)2  0.0041840  0.1441121   0.029
## sum_exp_100_quint5:factor(wave_id)2  0.0010346  0.1442009   0.007
## 
## Correlation of Fixed Effects:
##             (Intr) sm__100_2 sm__100_3 sm__100_4 sm__100_5 fc(_)2 s__100_2:
## sm_xp_100_2 -0.021                                                         
## sm_xp_100_3 -0.021  0.499                                                  
## sm_xp_100_4 -0.021  0.499     0.499                                        
## sm_xp_100_5 -0.021  0.499     0.498     0.498                              
## fctr(wv_d)2 -0.446  0.023     0.023     0.023     0.023                    
## s__100_2:(_  0.014 -0.660    -0.330    -0.329    -0.329    -0.035          
## s__100_3:(_  0.014 -0.330    -0.660    -0.329    -0.329    -0.035  0.499   
## s__100_4:(_  0.014 -0.329    -0.329    -0.660    -0.329    -0.035  0.499   
## s__100_5:(_  0.014 -0.329    -0.329    -0.329    -0.660    -0.035  0.499   
##             s__100_3: s__100_4:
## sm_xp_100_2                    
## sm_xp_100_3                    
## sm_xp_100_4                    
## sm_xp_100_5                    
## fctr(wv_d)2                    
## s__100_2:(_                    
## s__100_3:(_                    
## s__100_4:(_  0.499             
## s__100_5:(_  0.498     0.498
```

### Relative Exposure


```r
### Linear exposure
lmer_rel_exp100_ln <- lmer(mvpa ~ rel_exp_100*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp100_ln)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_100 * factor(wave_id) + (1 | date_time) + (1 |  
##     interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11329514
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5545 -0.6055 -0.0512  0.4948  5.3136 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 403.4    20.08   
##  interact_id (Intercept) 613.0    24.76   
##  Residual                635.7    25.21   
## Number of obs: 1218735, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                              Estimate Std. Error t value
## (Intercept)                   34.4563     2.2739   15.15
## rel_exp_100                   33.1501     0.6910   47.98
## factor(wave_id)2             -11.8455     2.1775   -5.44
## rel_exp_100:factor(wave_id)2  26.6548     0.9585   27.81
## 
## Correlation of Fixed Effects:
##             (Intr) rl__100 fc(_)2
## rel_exp_100 -0.008               
## fctr(wv_d)2 -0.456  0.006        
## r__100:(_)2  0.005 -0.699  -0.013
```

```r
### Quintiles of exposure
lmer_rel_exp100_q <- lmer(mvpa ~ rel_exp_100_quint*factor(wave_id) + (1 | date_time) + (1 | interact_id), data = data_exposure)
summary(lmer_rel_exp100_q)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_100_quint * factor(wave_id) + (1 | date_time) +  
##     (1 | interact_id)
##    Data: data_exposure
## 
## REML criterion at convergence: 11339177
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.6691 -0.6157 -0.0702  0.4864  5.2749 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 396.7    19.92   
##  interact_id (Intercept) 616.7    24.83   
##  Residual                640.8    25.31   
## Number of obs: 1218735, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                       Estimate Std. Error t value
## (Intercept)                          3.560e+01  2.270e+00  15.683
## rel_exp_100_quint2                   1.124e-02  9.576e-02   0.117
## rel_exp_100_quint3                   1.929e-02  9.581e-02   0.201
## rel_exp_100_quint4                   2.920e-02  9.586e-02   0.305
## rel_exp_100_quint5                   3.902e-02  9.592e-02   0.407
## factor(wave_id)2                    -1.018e+01  2.161e+00  -4.711
## rel_exp_100_quint2:factor(wave_id)2  7.928e-04  1.463e-01   0.005
## rel_exp_100_quint3:factor(wave_id)2  5.925e-03  1.464e-01   0.040
## rel_exp_100_quint4:factor(wave_id)2  4.373e-03  1.464e-01   0.030
## rel_exp_100_quint5:factor(wave_id)2  6.587e-04  1.465e-01   0.004
## 
## Correlation of Fixed Effects:
##             (Intr) rl__100_2 rl__100_3 rl__100_4 rl__100_5 fc(_)2 r__100_2:
## rl_xp_100_2 -0.021                                                         
## rl_xp_100_3 -0.021  0.499                                                  
## rl_xp_100_4 -0.021  0.499     0.499                                        
## rl_xp_100_5 -0.021  0.499     0.498     0.498                              
## fctr(wv_d)2 -0.453  0.022     0.022     0.022     0.022                    
## r__100_2:(_  0.014 -0.655    -0.327    -0.327    -0.326    -0.034          
## r__100_3:(_  0.014 -0.327    -0.655    -0.326    -0.326    -0.034  0.499   
## r__100_4:(_  0.014 -0.327    -0.326    -0.655    -0.326    -0.034  0.499   
## r__100_5:(_  0.014 -0.326    -0.326    -0.326    -0.655    -0.034  0.499   
##             r__100_3: r__100_4:
## rl_xp_100_2                    
## rl_xp_100_3                    
## rl_xp_100_4                    
## rl_xp_100_5                    
## fctr(wv_d)2                    
## r__100_2:(_                    
## r__100_3:(_                    
## r__100_4:(_  0.499             
## r__100_5:(_  0.498     0.498
```
