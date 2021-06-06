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
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.1.2     ✓ dplyr   1.0.6
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
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
library(rstatix)
```

```
## 
## Attaching package: 'rstatix'
```

```
## The following object is masked from 'package:stats':
## 
##     filter
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
## 
## ── Column specification ────────────────────────────────────────────────────────
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
## ℹ Use `spec()` for the full column specifications.
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
## 
## ── Column specification ────────────────────────────────────────────────────────
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
## ℹ Use `spec()` for the full column specifications.
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
## 
## ── Column specification ────────────────────────────────────────────────────────
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
## ℹ Use `spec()` for the full column specifications.
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
                      rel_exp_70 = sum(exposed_70)/n()*100,
                      rel_exp_50 = sum(exposed_50)/n()*100,
                      rel_exp_100 = sum(exposed_100)/n()*100,
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
## `summarise()` has grouped output by 'interact_id', 'wave_id', 'date'. You can override using the `.groups` argument.
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
data_exposure$sum_exp_70 <- if_else(data_exposure$sum_exp_70 > 500, NA_real_, data_exposure$sum_exp_70) #1264068
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
summary(data_exposure$sum_exp_70)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    2.00   21.72   13.00  497.00    6029
```

```r
summary(data_exposure$sum_exp_50)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    2.00   19.33   10.00  494.00     664
```

```r
summary(data_exposure$sum_exp_100)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    3.00   25.94   18.00  495.00    8588
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
##   0.0000   0.0000   0.4057   4.2981   2.3684 100.0000
```

```r
### Removing participants with more than 40% exposure
data_exposure$rel_exp_70 <- if_else(data_exposure$rel_exp_70 > 40, NA_real_, data_exposure$rel_exp_70)
data_exposure$rel_exp_50 <- if_else(data_exposure$rel_exp_50 > 40, NA_real_, data_exposure$rel_exp_50)
data_exposure$rel_exp_100 <- if_else(data_exposure$rel_exp_100 > 40, NA_real_, data_exposure$rel_exp_100)

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
summary(data_exposure$rel_exp_70)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.35    2.53    2.05   38.65   39594
```

```r
summary(data_exposure$rel_exp_50)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.271   2.250   1.689  39.041   29598
```

```r
summary(data_exposure$rel_exp_100)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    0.53    3.25    2.87   40.00   45333
```

### Creating a days variable 

```r
data_exposure <- data_exposure[order(data_exposure$date, data_exposure$interact_id), ]

data_exposure$time_seq <- as.numeric(data_exposure$date_time)
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

![](regression_analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Histograms of Moderate to vigorous PA 


```r
summary(data_exposure$mvpa)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    0.00   11.00   26.00   36.12   51.00  267.00
```

```r
sd(data_exposure$mvpa)
```

```
## [1] 35.15078
```

```r
waves_pa <- data_exposure %>%
              group_by(wave_id) %>%
                get_summary_stats(mvpa, type = "mean_sd")
waves_pa
```

```
## # A tibble: 2 x 5
##   wave_id variable      n  mean    sd
##     <dbl> <chr>     <dbl> <dbl> <dbl>
## 1       1 mvpa     708729  35.1  35.2
## 2       2 mvpa     555339  37.4  35.0
```

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

![](regression_analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

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

![](regression_analysis_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

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

![](regression_analysis_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

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
   <td style="text-align:center;"> 0.59 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> [0.57, 0.60] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rel_exp_70 × factor(wave_id)2 </td>
   <td style="text-align:center;">  </td>
   <td style="text-align:center;"> -0.21 </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px">  </td>
   <td style="text-align:center;box-shadow: 0px 1px"> [-0.23, -0.19] </td>
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

### Absolute exposure with a 70 meter buffer and covariates - Random intercepts

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
## (Intercept)                               31.505701  12.321418   2.557
## sum_exp_70                                 0.115439   0.001296  89.041
## factor(wave_id)2                           0.932910   3.185072   0.293
## mean_temp_c                               -0.544184   0.301103  -1.807
## total_precip_mm                           -0.013153   0.255324  -0.052
## speed_gust_km_h                            0.070658   0.136200   0.519
## gender_recode2Transgender                -22.910528  16.214843  -1.413
## gender_recode2Woman                       -9.002616   4.034207  -2.232
## income_recode100_200                      -0.043974  11.056684  -0.004
## income_recode20_49 999                    12.140214  11.492284   1.056
## income_recode200+                         19.114407  24.883114   0.768
## income_recode50_99 999                    -0.448352  10.735386  -0.042
## income_recodeDon't know/prefer no answer   4.633076  12.895924   0.359
## age_recode30_39                            7.741094   6.329549   1.223
## age_recode40_49                            9.105662   6.714631   1.356
## age_recode50_64                            8.883314   7.574730   1.173
## age_recode65+                             -0.334705   7.983107  -0.042
## sum_exp_70:factor(wave_id)2               -0.157591   0.001728 -91.194
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_abs_exp70_ln_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    16.3586099 19.70978672
## .sig02                                    18.9250478 24.06003184
## .sigma                                    23.4642380 23.54185914
## (Intercept)                                8.0899947 54.91238649
## sum_exp_70                                 0.1128907  0.11797280
## factor(wave_id)2                          -5.2762227  7.14130614
## mean_temp_c                               -1.1309338  0.04299357
## total_precip_mm                           -0.5108664  0.48455512
## speed_gust_km_h                           -0.1948404  0.33615706
## gender_recode2Transgender                -53.4858433  7.66727836
## gender_recode2Woman                      -16.6096169 -1.39473351
## income_recode100_200                     -20.8935318 20.80595917
## income_recode20_49 999                    -9.5310693 33.81118953
## income_recode200+                        -27.8086705 66.03640748
## income_recode50_99 999                   -20.6921333 19.79557306
## income_recodeDon't know/prefer no answer -19.6851952 28.95088118
## age_recode30_39                           -4.1950741 19.67659189
## age_recode40_49                           -3.5560170 21.76787258
## age_recode50_64                           -5.4010251 23.16659050
## age_recode65+                            -15.3882638 14.71964410
## sum_exp_70:factor(wave_id)2               -0.1609702 -0.15419636
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
### Absolute Model Continuous - with offset
lmer_abs_exp70_ln_cov_off <- lmer(mvpa ~ sum_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure, offset = minutes)
summary(lmer_abs_exp70_ln_cov_off)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ sum_exp_70 * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
##  Offset: minutes
## 
## REML criterion at convergence: 8905394
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5960 -0.6736 -0.1465  0.5165  5.5502 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 17904    133.8   
##  interact_id (Intercept) 25086    158.4   
##  Residual                17904    133.8   
## Number of obs: 704845, groups:  date_time, 225; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                              -3.060e+02  8.877e+01  -3.447
## sum_exp_70                               -3.466e-01  7.382e-03 -46.951
## factor(wave_id)2                          4.904e+01  2.355e+01   2.082
## mean_temp_c                              -6.203e+00  2.224e+00  -2.789
## total_precip_mm                           2.564e-01  1.889e+00   0.136
## speed_gust_km_h                           2.730e-01  1.008e+00   0.271
## gender_recode2Transgender                -1.235e+02  1.155e+02  -1.069
## gender_recode2Woman                       1.037e+01  2.874e+01   0.361
## income_recode100_200                     -3.708e+00  7.876e+01  -0.047
## income_recode20_49 999                   -4.539e+01  8.187e+01  -0.554
## income_recode200+                        -1.682e+02  1.773e+02  -0.949
## income_recode50_99 999                   -1.460e+00  7.647e+01  -0.019
## income_recodeDon't know/prefer no answer -1.709e+01  9.186e+01  -0.186
## age_recode30_39                          -6.430e+01  4.509e+01  -1.426
## age_recode40_49                          -9.346e+01  4.783e+01  -1.954
## age_recode50_64                          -1.550e+02  5.396e+01  -2.873
## age_recode65+                            -1.219e+02  5.687e+01  -2.144
## sum_exp_70:factor(wave_id)2               3.980e-01  9.840e-03  40.449
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_abs_exp70_ln_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    16.3586099 19.70978672
## .sig02                                    18.9250478 24.06003184
## .sigma                                    23.4642380 23.54185914
## (Intercept)                                8.0899947 54.91238649
## sum_exp_70                                 0.1128907  0.11797280
## factor(wave_id)2                          -5.2762227  7.14130614
## mean_temp_c                               -1.1309338  0.04299357
## total_precip_mm                           -0.5108664  0.48455512
## speed_gust_km_h                           -0.1948404  0.33615706
## gender_recode2Transgender                -53.4858433  7.66727836
## gender_recode2Woman                      -16.6096169 -1.39473351
## income_recode100_200                     -20.8935318 20.80595917
## income_recode20_49 999                    -9.5310693 33.81118953
## income_recode200+                        -27.8086705 66.03640748
## income_recode50_99 999                   -20.6921333 19.79557306
## income_recodeDon't know/prefer no answer -19.6851952 28.95088118
## age_recode30_39                           -4.1950741 19.67659189
## age_recode40_49                           -3.5560170 21.76787258
## age_recode50_64                           -5.4010251 23.16659050
## age_recode65+                            -15.3882638 14.71964410
## sum_exp_70:factor(wave_id)2               -0.1609702 -0.15419636
```

```r
broom.mixed::glance(lmer_abs_exp70_ln_cov_off)
```

```
## # A tibble: 1 x 6
##   sigma    logLik      AIC      BIC REMLcrit df.residual
##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
## 1  134. -4452697. 8905436. 8905677. 8905394.      704824
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
## (Intercept)                               32.230502  12.128291   2.657
## sum_exp_70_quint2                          0.010197   0.097314   0.105
## sum_exp_70_quint3                          0.018344   0.097369   0.188
## sum_exp_70_quint4                          0.029048   0.097419   0.298
## sum_exp_70_quint5                          0.036122   0.097481   0.371
## factor(wave_id)2                          -1.700241   3.176742  -0.535
## mean_temp_c                               -0.505224   0.300002  -1.684
## total_precip_mm                           -0.017584   0.254380  -0.069
## speed_gust_km_h                            0.047873   0.135696   0.353
## gender_recode2Transgender                -22.086227  15.878538  -1.391
## gender_recode2Woman                       -8.393268   3.950564  -2.125
## income_recode100_200                      -0.480690  10.827364  -0.044
## income_recode20_49 999                    12.874443  11.253915   1.144
## income_recode200+                         17.045033  24.367068   0.700
## income_recode50_99 999                     0.262062  10.512714   0.025
## income_recodeDon't know/prefer no answer   4.264155  12.628462   0.338
## age_recode30_39                            8.184408   6.198307   1.320
## age_recode40_49                            9.797109   6.575330   1.490
## age_recode50_64                            8.758135   7.417638   1.181
## age_recode65+                             -0.606259   7.817579  -0.078
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
confint.merMod(lmer_abs_exp70_q_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    16.2982110 19.63599613
## .sig02                                    18.5325473 23.56128124
## .sigma                                    23.6208880 23.69902764
## (Intercept)                                9.1764820 55.27522595
## sum_exp_70_quint2                         -0.1805335  0.20092864
## sum_exp_70_quint3                         -0.1724940  0.20918466
## sum_exp_70_quint4                         -0.1618875  0.21998575
## sum_exp_70_quint5                         -0.1549347  0.22718030
## factor(wave_id)2                          -7.8930500  4.49198405
## mean_temp_c                               -1.0898150  0.07981193
## total_precip_mm                           -0.5134589  0.47827994
## speed_gust_km_h                           -0.2166370  0.31239307
## gender_recode2Transgender                -52.0279311  7.85787360
## gender_recode2Woman                      -15.8427135 -0.94302790
## income_recode100_200                     -20.8981083 19.93722127
## income_recode20_49 999                    -8.3477792 34.09622029
## income_recode200+                        -28.9056113 62.99482910
## income_recode50_99 999                   -19.5621631 20.08642941
## income_recodeDon't know/prefer no answer -19.5501893 28.07799696
## age_recode30_39                           -3.5045067 19.87259627
## age_recode40_49                           -2.6021247 22.19681802
## age_recode50_64                           -5.2302741 22.74536289
## age_recode65+                            -15.3479559 14.13618689
## sum_exp_70_quint2:factor(wave_id)2        -0.4673875  0.47409214
## sum_exp_70_quint3:factor(wave_id)2        -0.4633727  0.47873093
## sum_exp_70_quint4:factor(wave_id)2        -0.4748219  0.46774498
## sum_exp_70_quint5:factor(wave_id)2        -0.4731205  0.46990448
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

```r
### Absolute Model Quintiles - with offset
lmer_abs_exp70_q_cov_off <- lmer(mvpa ~ sum_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure, offset = minutes)
summary(lmer_abs_exp70_q_cov_off)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## mvpa ~ sum_exp_70_quint * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
##  Offset: minutes
## 
## REML criterion at convergence: 8907655
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5642 -0.6740 -0.1554  0.5125  5.5906 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 17864    133.7   
##  interact_id (Intercept) 25352    159.2   
##  Residual                17963    134.0   
## Number of obs: 704845, groups:  date_time, 225; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                              -304.80160   89.05878  -3.422
## sum_exp_70_quint2                          -0.18394    0.55125  -0.334
## sum_exp_70_quint3                          -0.32689    0.55156  -0.593
## sum_exp_70_quint4                          -0.46742    0.55184  -0.847
## sum_exp_70_quint5                          -0.65615    0.55219  -1.188
## factor(wave_id)2                           54.62152   23.54003   2.320
## mean_temp_c                                -6.47077    2.22105  -2.913
## total_precip_mm                             0.27176    1.88649   0.144
## speed_gust_km_h                             0.32390    1.00639   0.322
## gender_recode2Transgender                -124.97273  116.11770  -1.076
## gender_recode2Woman                         9.03522   28.88800   0.313
## income_recode100_200                       -2.87103   79.17915  -0.036
## income_recode20_49 999                    -47.42349   82.29926  -0.576
## income_recode200+                        -162.03697  178.19088  -0.909
## income_recode50_99 999                     -4.20599   76.87857  -0.055
## income_recodeDon't know/prefer no answer  -15.50156   92.35002  -0.168
## age_recode30_39                           -66.32408   45.32471  -1.463
## age_recode40_49                           -97.05744   48.08301  -2.019
## age_recode50_64                          -155.22142   54.24347  -2.862
## age_recode65+                            -121.23098   57.16656  -2.121
## sum_exp_70_quint2:factor(wave_id)2          0.03915    1.36052   0.029
## sum_exp_70_quint3:factor(wave_id)2          0.04233    1.36142   0.031
## sum_exp_70_quint4:factor(wave_id)2          0.12773    1.36209   0.094
## sum_exp_70_quint5:factor(wave_id)2          0.10699    1.36275   0.079
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_abs_exp70_q_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    16.2982110 19.63599613
## .sig02                                    18.5325473 23.56128124
## .sigma                                    23.6208880 23.69902764
## (Intercept)                                9.1764820 55.27522595
## sum_exp_70_quint2                         -0.1805335  0.20092864
## sum_exp_70_quint3                         -0.1724940  0.20918466
## sum_exp_70_quint4                         -0.1618875  0.21998575
## sum_exp_70_quint5                         -0.1549347  0.22718030
## factor(wave_id)2                          -7.8930500  4.49198405
## mean_temp_c                               -1.0898150  0.07981193
## total_precip_mm                           -0.5134589  0.47827994
## speed_gust_km_h                           -0.2166370  0.31239307
## gender_recode2Transgender                -52.0279311  7.85787360
## gender_recode2Woman                      -15.8427135 -0.94302790
## income_recode100_200                     -20.8981083 19.93722127
## income_recode20_49 999                    -8.3477792 34.09622029
## income_recode200+                        -28.9056113 62.99482910
## income_recode50_99 999                   -19.5621631 20.08642941
## income_recodeDon't know/prefer no answer -19.5501893 28.07799696
## age_recode30_39                           -3.5045067 19.87259627
## age_recode40_49                           -2.6021247 22.19681802
## age_recode50_64                           -5.2302741 22.74536289
## age_recode65+                            -15.3479559 14.13618689
## sum_exp_70_quint2:factor(wave_id)2        -0.4673875  0.47409214
## sum_exp_70_quint3:factor(wave_id)2        -0.4633727  0.47873093
## sum_exp_70_quint4:factor(wave_id)2        -0.4748219  0.46774498
## sum_exp_70_quint5:factor(wave_id)2        -0.4731205  0.46990448
```

```r
broom.mixed::glance(lmer_abs_exp70_q_cov_off)
```

```
## # A tibble: 1 x 6
##   sigma    logLik      AIC      BIC REMLcrit df.residual
##   <dbl>     <dbl>    <dbl>    <dbl>    <dbl>       <int>
## 1  134. -4453827. 8907709. 8908019. 8907655.      704818
```

### Absolute exposure with a 70 meter buffer and covariates - Random slopes

```r
### Absolute Model Continuous
#lmer_abs_exp70_ln_cov_rs <- lmer(mvpa ~ sum_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))

#summary(lmer_abs_exp70_ln_cov_rs)
#confint.merMod(lmer_abs_exp70_ln_cov_rs)
#broom.mixed::glance(lmer_abs_exp70_ln_cov_rs)

### Absolute Model Continuous - with offset
#lmer_abs_exp70_ln_cov_off_rs <- lmer(mvpa ~ sum_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, offset = minutes, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_abs_exp70_ln_cov_off_rs)
#confint.merMod(lmer_abs_exp70_ln_cov_off_rs)
#broom.mixed::glance(lmer_abs_exp70_ln_cov_off_rs)

### Absolute Model Quintiles
#lmer_abs_exp70_q_cov_rs <- lmer(mvpa ~ sum_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_abs_exp70_q_cov_rs)
#confint.merMod(lmer_abs_exp70_q_cov_rs)
#broom.mixed::glance(lmer_abs_exp70_q_cov_rs)

### Absolute Model Quintiles - with offset
#lmer_abs_exp70_q_cov_off_rs <- lmer(mvpa ~ sum_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, offset = minutes, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_abs_exp70_q_cov_off_rs)
#confint.merMod(lmer_abs_exp70_q_cov_off_rs)
#broom.mixed::glance(lmer_abs_exp70_q_cov_off_rs)
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
## REML criterion at convergence: 11380651
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
##                               Estimate Std. Error t value
## (Intercept)                  34.469967   2.260092  15.252
## rel_exp_70                    0.401005   0.008087  49.585
## factor(wave_id)2            -11.629958   2.164780  -5.372
## rel_exp_70:factor(wave_id)2   0.289485   0.011075  26.137
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

### Relative exposure with a 70 meter buffer and covariates - Random intercepts


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
## REML criterion at convergence: 6332248
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
##                                            Estimate Std. Error t value
## (Intercept)                               32.419049  12.564212   2.580
## rel_exp_70                                 0.460871   0.008572  53.766
## factor(wave_id)2                           1.064848   3.275743   0.325
## mean_temp_c                               -0.657558   0.304750  -2.158
## total_precip_mm                            0.024473   0.257881   0.095
## speed_gust_km_h                            0.097748   0.138122   0.708
## gender_recode2Transgender                -23.739208  16.594549  -1.431
## gender_recode2Woman                       -9.986631   4.128875  -2.419
## income_recode100_200                      -0.967093  11.315568  -0.085
## income_recode20_49 999                    12.759202  11.761301   1.085
## income_recode200+                         19.629923  25.465558   0.771
## income_recode50_99 999                    -0.277290  10.986738  -0.025
## income_recodeDon't know/prefer no answer   4.214822  13.197798   0.319
## age_recode30_39                            8.070241   6.477769   1.246
## age_recode40_49                           11.102231   6.872459   1.615
## age_recode50_64                            9.756712   7.752126   1.259
## age_recode65+                             -0.550469   8.170017  -0.067
## rel_exp_70:factor(wave_id)2               -0.352994   0.016688 -21.152
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_rel_exp_ln_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    16.4794344 19.88149358
## .sig02                                    19.3658906 24.62243682
## .sigma                                    23.5805996 23.65939354
## (Intercept)                                8.5461753 56.28012978
## rel_exp_70                                 0.4440526  0.47765378
## factor(wave_id)2                          -5.3210355  7.44910175
## mean_temp_c                               -1.2512946 -0.06323048
## total_precip_mm                           -0.4782011  0.52712017
## speed_gust_km_h                           -0.1714782  0.36697418
## gender_recode2Transgender                -55.0275893  7.55322303
## gender_recode2Woman                      -17.7712780 -2.20038723
## income_recode100_200                     -22.3032333 20.36965036
## income_recode20_49 999                    -9.4178931 34.93584428
## income_recode200+                        -28.3885316 67.64646984
## income_recode50_99 999                   -20.9935998 20.43918068
## income_recodeDon't know/prefer no answer -20.6708586 29.10023645
## age_recode30_39                           -4.1442488 20.28472832
## age_recode40_49                           -1.8563636 24.06111450
## age_recode50_64                           -4.8611011 24.37352742
## age_recode65+                            -15.9549530 14.85573917
## rel_exp_70:factor(wave_id)2               -0.3855688 -0.32014903
```

```r
### Linear exposure - with offset
lmer_rel_exp_ln_cov_off <- lmer(mvpa ~ rel_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure, offset = minutes)
summary(lmer_rel_exp_ln_cov_off)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mvpa ~ rel_exp_70 * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
##  Offset: minutes
## 
## REML criterion at convergence: 8726810
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7082 -0.6605 -0.1588  0.5108  5.6359 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 17585    132.6   
##  interact_id (Intercept) 25952    161.1   
##  Residual                17856    133.6   
## Number of obs: 690856, groups:  date_time, 222; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                              -299.24503   89.64184  -3.338
## rel_exp_70                                  1.83087    0.04850  37.749
## factor(wave_id)2                           51.75747   23.81152   2.174
## mean_temp_c                                -7.21377    2.21232  -3.261
## total_precip_mm                             0.48507    1.87519   0.259
## speed_gust_km_h                             0.45357    1.00441   0.452
## gender_recode2Transgender                -125.86233  117.48242  -1.071
## gender_recode2Woman                        -2.19498   29.22848  -0.075
## income_recode100_200                      -14.51122   80.10958  -0.181
## income_recode20_49 999                    -49.21078   83.26604  -0.591
## income_recode200+                        -153.09299  180.28403  -0.849
## income_recode50_99 999                     -6.29076   77.78196  -0.081
## income_recodeDon't know/prefer no answer  -15.42059   93.43489  -0.165
## age_recode30_39                           -60.17935   45.85750  -1.312
## age_recode40_49                           -77.29208   48.65113  -1.589
## age_recode50_64                          -142.01229   54.88108  -2.588
## age_recode65+                            -116.59198   57.83830  -2.016
## rel_exp_70:factor(wave_id)2                 3.49363    0.09443  36.996
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_rel_exp_ln_cov_off)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %      97.5 %
## .sig01                                    119.905451  144.598353
## .sig02                                    137.115197  174.331507
## .sigma                                    133.405223  133.850990
## (Intercept)                              -469.604031 -128.927764
## rel_exp_70                                  1.735726    1.925844
## factor(wave_id)2                            5.340948   98.164564
## mean_temp_c                               -11.524332   -2.900063
## total_precip_mm                            -3.169979    4.139986
## speed_gust_km_h                            -1.504374    2.411087
## gender_recode2Transgender                -347.379858   95.674250
## gender_recode2Woman                       -57.305034   52.923451
## income_recode100_200                     -165.557987  136.554182
## income_recode20_49 999                   -206.215803  107.799775
## income_recode200+                        -493.029560  186.864906
## income_recode50_99 999                   -152.957264  140.376752
## income_recodeDon't know/prefer no answer -191.592903  160.772214
## age_recode30_39                          -146.647642   26.292904
## age_recode40_49                          -169.032440   14.443805
## age_recode50_64                          -245.497700  -38.527829
## age_recode65+                            -225.651177   -7.528369
## rel_exp_70:factor(wave_id)2                 3.308984    3.679158
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
## (Intercept)                               32.053454  12.535163   2.557
## rel_exp_70_quint2                          0.010354   0.097921   0.106
## rel_exp_70_quint3                          0.017947   0.097976   0.183
## rel_exp_70_quint4                          0.028390   0.098026   0.290
## rel_exp_70_quint5                          0.035381   0.098088   0.361
## factor(wave_id)2                           0.263504   3.238448   0.081
## mean_temp_c                               -0.579268   0.300994  -1.925
## total_precip_mm                            0.004761   0.254677   0.019
## speed_gust_km_h                            0.092489   0.136405   0.678
## gender_recode2Transgender                -23.363715  16.623619  -1.405
## gender_recode2Woman                       -9.238491   4.136081  -2.234
## income_recode100_200                      -0.526550  11.335417  -0.046
## income_recode20_49 999                    12.895443  11.781936   1.095
## income_recode200+                         17.638534  25.510209   0.691
## income_recode50_99 999                     0.138276  11.006010   0.013
## income_recodeDon't know/prefer no answer   3.898057  13.220949   0.295
## age_recode30_39                            8.113319   6.489124   1.250
## age_recode40_49                           10.703042   6.884514   1.555
## age_recode50_64                            8.939917   7.765709   1.151
## age_recode65+                             -0.939608   8.184318  -0.115
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

```r
confint.merMod(lmer_rel_exp_q_cov)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %       97.5 %
## .sig01                                    16.2746868 19.634390588
## .sig02                                    19.4002250 24.665392948
## .sigma                                    23.6303290 23.709288518
## (Intercept)                                8.2399769 55.856063901
## rel_exp_70_quint2                         -0.1815660  0.202274864
## rel_exp_70_quint3                         -0.1740805  0.209977442
## rel_exp_70_quint4                         -0.1637356  0.220517836
## rel_exp_70_quint5                         -0.1568650  0.227629610
## factor(wave_id)2                          -6.0493924  6.575726213
## mean_temp_c                               -1.1657332  0.007720801
## total_precip_mm                           -0.4916779  0.501180029
## speed_gust_km_h                           -0.1733959  0.358378793
## gender_recode2Transgender                -54.7074845  7.983077354
## gender_recode2Woman                      -17.0369970 -1.438908021
## income_recode100_200                     -21.9002618 20.847564759
## income_recode20_49 999                    -9.3205408 35.111104063
## income_recode200+                        -30.4638738 65.739710181
## income_recode50_99 999                   -20.6142750 20.891265944
## income_recodeDon't know/prefer no answer -21.0314173 28.827088523
## age_recode30_39                           -4.1226919 20.349155738
## age_recode40_49                           -2.2781279 23.684863213
## age_recode50_64                           -5.7035068 23.582404161
## age_recode65+                            -16.3713864 14.493288867
## rel_exp_70_quint2:factor(wave_id)2        -0.4792647  0.489243175
## rel_exp_70_quint3:factor(wave_id)2        -0.4721458  0.496994376
## rel_exp_70_quint4:factor(wave_id)2        -0.4829200  0.486704110
## rel_exp_70_quint5:factor(wave_id)2        -0.4798218  0.490277189
```

```r
### Quintiles of exposure - with offset
lmer_rel_exp_q_cov_off <- lmer(mvpa ~ rel_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (1 | date_time) + (1 | interact_id), data = data_exposure, offset = minutes)
summary(lmer_rel_exp_q_cov_off)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## mvpa ~ rel_exp_70_quint * factor(wave_id) + mean_temp_c + total_precip_mm +  
##     speed_gust_km_h + gender_recode2 + income_recode + age_recode +  
##     (1 | date_time) + (1 | interact_id)
##    Data: data_exposure
##  Offset: minutes
## 
## REML criterion at convergence: 8732013
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5322 -0.6674 -0.1572  0.5068  5.6280 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 18500    136.0   
##  interact_id (Intercept) 26706    163.4   
##  Residual                17992    134.1   
## Number of obs: 690856, groups:  date_time, 222; interact_id, 134
## 
## Fixed effects:
##                                            Estimate Std. Error t value
## (Intercept)                              -290.50767   91.23853  -3.184
## rel_exp_70_quint2                          -0.18360    0.55490  -0.331
## rel_exp_70_quint3                          -0.32308    0.55521  -0.582
## rel_exp_70_quint4                          -0.46034    0.55550  -0.829
## rel_exp_70_quint5                          -0.65123    0.55585  -1.172
## factor(wave_id)2                           65.08698   24.43618   2.664
## mean_temp_c                                -7.32679    2.26882  -3.229
## total_precip_mm                             0.41259    1.92331   0.215
## speed_gust_km_h                             0.48532    1.03019   0.471
## gender_recode2Transgender                -137.00331  119.17686  -1.150
## gender_recode2Woman                        -2.58305   29.64983  -0.087
## income_recode100_200                      -10.79640   81.26512  -0.133
## income_recode20_49 999                    -47.03479   84.46716  -0.557
## income_recode200+                        -157.41210  182.88438  -0.861
## income_recode50_99 999                     -2.08202   78.90393  -0.026
## income_recodeDon't know/prefer no answer  -20.08025   94.78264  -0.212
## age_recode30_39                           -63.95213   46.51884  -1.375
## age_recode40_49                           -80.09470   49.35277  -1.623
## age_recode50_64                          -148.25622   55.67261  -2.663
## age_recode65+                            -125.84064   58.67237  -2.145
## rel_exp_70_quint2:factor(wave_id)2          0.02795    1.40013   0.020
## rel_exp_70_quint3:factor(wave_id)2          0.02526    1.40104   0.018
## rel_exp_70_quint4:factor(wave_id)2          0.10805    1.40174   0.077
## rel_exp_70_quint5:factor(wave_id)2          0.07655    1.40243   0.055
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
##     vcov(x)        if you need it
```

```r
confint.merMod(lmer_rel_exp_q_cov_off)
```

```
## Computing profile confidence intervals ...
```

```
##                                                2.5 %       97.5 %
## .sig01                                    122.985289  148.3121164
## .sig02                                    139.100172  176.8534931
## .sigma                                    133.908959  134.3564138
## (Intercept)                              -463.929962 -117.1293580
## rel_exp_70_quint2                          -1.271178    0.9039812
## rel_exp_70_quint3                          -1.411280    0.7651096
## rel_exp_70_quint4                          -1.549105    0.6283926
## rel_exp_70_quint5                          -1.740674    0.4381900
## factor(wave_id)2                           17.454733  112.7129077
## mean_temp_c                               -11.747397   -2.9029485
## total_precip_mm                            -3.336250    4.1612558
## speed_gust_km_h                            -1.522867    2.4930633
## gender_recode2Transgender                -361.725609   87.7376261
## gender_recode2Woman                       -58.490271   53.3318798
## income_recode100_200                     -164.028589  142.4544526
## income_recode20_49 999                   -206.311002  112.2476690
## income_recode200+                        -502.266495  187.4638104
## income_recode50_99 999                   -150.869778  146.7081126
## income_recodeDon't know/prefer no answer -198.801373  158.6615237
## age_recode30_39                          -151.670753   23.7712620
## age_recode40_49                          -173.160926   12.9691730
## age_recode50_64                          -253.238322  -43.2745552
## age_recode65+                            -236.477317  -15.1998089
## rel_exp_70_quint2:factor(wave_id)2         -2.716234    2.7721321
## rel_exp_70_quint3:factor(wave_id)2         -2.720720    2.7712284
## rel_exp_70_quint4:factor(wave_id)2         -2.639292    2.8553987
## rel_exp_70_quint5:factor(wave_id)2         -2.672134    2.8252487
```

### Relative exposure with a 70 meter buffer and covariates - Random Slopes


```r
### Linear exposure
#lmer_rel_exp_ln_cov_rs <- lmer(mvpa ~ rel_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_rel_exp_ln_cov_rs)
#confint.merMod(lmer_rel_exp_ln_cov_rs)

### Linear exposure - with offset
#lmer_rel_exp_ln_cov_off_rs <- lmer(mvpa ~ rel_exp_70*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_rel_exp_ln_cov_off_rs)
#confint.merMod(lmer_rel_exp_ln_cov_off_rs)

### Quintiles of exposure
#lmer_rel_exp_q_cov_rs <- lmer(mvpa ~ rel_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_rel_exp_q_cov_rs)
#confint.merMod(lmer_rel_exp_q_cov_rs)

### Quintiles of exposure - with offset
#lmer_rel_exp_q_cov_off_rs <- lmer(mvpa ~ rel_exp_70_quint*factor(wave_id) + mean_temp_c + total_precip_mm + speed_gust_km_h + gender_recode2 + income_recode + age_recode + (time_seq | interact_id) + (1 | interact_id), data = data_exposure, control = lmerControl(optimizer ="Nelder_Mead"))
#summary(lmer_rel_exp_q_cov_off_rs)
#confint.merMod(lmer_rel_exp_q_cov_off_rs)
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
## (Intercept)                 34.308043   2.202988  15.573
## sum_exp_50                   0.117191   0.001324  88.519
## factor(wave_id)2            -9.219587   2.080768  -4.431
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
##  interact_id (Intercept) 602.9    24.55   
##  Residual                658.1    25.65   
## Number of obs: 1263404, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                      Estimate Std. Error t value
## (Intercept)                         3.576e+01  2.221e+00  16.105
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
## REML criterion at convergence: 11498309
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5642 -0.6128 -0.0560  0.4886  5.2598 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 391.6    19.79   
##  interact_id (Intercept) 605.3    24.60   
##  Residual                647.5    25.45   
## Number of obs: 1234470, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                               Estimate Std. Error t value
## (Intercept)                  34.438752   2.251422  15.296
## rel_exp_50                    0.482402   0.009188  52.501
## factor(wave_id)2            -11.239668   2.144739  -5.241
## rel_exp_50:factor(wave_id)2   0.272436   0.011994  22.714
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
## (Intercept)                  34.110667   2.201275  15.496
## sum_exp_100                   0.083612   0.000911  91.778
## factor(wave_id)2             -9.491367   2.080369  -4.562
## sum_exp_100:factor(wave_id)2 -0.044812   0.001111 -40.341
## 
## Correlation of Fixed Effects:
##             (Intr) sm__100 fc(_)2
## sum_exp_100 -0.007               
## fctr(wv_d)2 -0.451  0.004        
## s__100:(_)2  0.004 -0.798  -0.008
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
## (Intercept)                         35.6183407  2.2124992  16.099
## sum_exp_100_quint2                   0.0115503  0.0950738   0.121
## sum_exp_100_quint3                   0.0196742  0.0951278   0.207
## sum_exp_100_quint4                   0.0302001  0.0951745   0.317
## sum_exp_100_quint5                   0.0395910  0.0952348   0.416
## factor(wave_id)2                    -9.2606343  2.0712199  -4.471
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
## REML criterion at convergence: 11329533
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5545 -0.6055 -0.0512  0.4948  5.3136 
## 
## Random effects:
##  Groups      Name        Variance Std.Dev.
##  date_time   (Intercept) 403.4    20.09   
##  interact_id (Intercept) 613.0    24.76   
##  Residual                635.7    25.21   
## Number of obs: 1218735, groups:  date_time, 345; interact_id, 210
## 
## Fixed effects:
##                                Estimate Std. Error t value
## (Intercept)                   34.456317   2.273935   15.15
## rel_exp_100                    0.331501   0.006910   47.98
## factor(wave_id)2             -11.845569   2.177568   -5.44
## rel_exp_100:factor(wave_id)2   0.266547   0.009585   27.81
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
