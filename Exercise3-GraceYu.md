Exercise 3 - Grace Yu
================

``` r
data_path <- "~/Dropbox/People Analytics/"
data <- read_parquet(paste0(data_path,"filtered_data.parquet"))
```

``` r
#employees are examiners, need to group into one
data <- data %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
turnover_data <- data %>%
  group_by(examiner_id) %>%
  summarize(in_year = first(earliest_date), out_year = first(latest_date), tc = first(tc), gender = first(gender), race = first(race), exited = ifelse(out_year<2017, 1, 0))

validation_set <- turnover_data %>%
  slice_sample(prop = 0.15)

training_set <- turnover_data %>%
  anti_join(validation_set)
```

    ## Joining with `by = join_by(examiner_id, in_year, out_year, tc, gender, race,
    ## exited)`

``` r
model_gender_tc <- lm(exited ~ 1 + factor(gender) + factor(tc), data = training_set)
tidy(model_gender_tc)
```

    ## # A tibble: 5 × 5
    ##   term               estimate std.error statistic  p.value
    ##   <chr>                 <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)          0.276     0.0172    16.1   2.34e-56
    ## 2 factor(gender)male   0.0235    0.0150     1.57  1.17e- 1
    ## 3 factor(tc)1700      -0.0777    0.0199    -3.90  9.83e- 5
    ## 4 factor(tc)2100       0.0159    0.0198     0.806 4.20e- 1
    ## 5 factor(tc)2400      -0.0979    0.0218    -4.49  7.46e- 6

``` r
summary(model_gender_tc)
```

    ## 
    ## Call:
    ## lm(formula = exited ~ 1 + factor(gender) + factor(tc), data = training_set)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3153 -0.2918 -0.2217  0.6847  0.8220 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         0.27591    0.01718  16.062  < 2e-16 ***
    ## factor(gender)male  0.02350    0.01499   1.568    0.117    
    ## factor(tc)1700     -0.07772    0.01993  -3.899 9.83e-05 ***
    ## factor(tc)2100      0.01592    0.01976   0.806    0.420    
    ## factor(tc)2400     -0.09795    0.02184  -4.486 7.46e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4337 on 4124 degrees of freedom
    ##   (673 observations deleted due to missingness)
    ## Multiple R-squared:  0.01315,    Adjusted R-squared:  0.0122 
    ## F-statistic: 13.74 on 4 and 4124 DF,  p-value: 3.912e-11

``` r
#Check prediction against validation set
model_pred <- predict(model_gender_tc, newdata = validation_set)
result <- data.frame(actuals = validation_set$exited, predictions = model_pred)
result <- result %>%
  mutate(predictions = if_else(predictions >= 0.5, 1, 0))
#Check false negatives and false positives
confusion_matrix_1 <- table(result$predictions, result$actuals)
table_gender_tc <- prop.table(confusion_matrix_1["0", "1"])
```

``` r
model_with_race <- lm(exited ~ 1 + factor(gender) + factor(tc) + factor(race), data = training_set)
tidy(model_with_race)
```

    ## # A tibble: 9 × 5
    ##   term                 estimate std.error statistic  p.value
    ##   <chr>                   <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)            0.212     0.0217     9.78  2.36e-22
    ## 2 factor(gender)male     0.0207    0.0150     1.38  1.67e- 1
    ## 3 factor(tc)1700        -0.0755    0.0199    -3.80  1.49e- 4
    ## 4 factor(tc)2100         0.0299    0.0200     1.50  1.34e- 1
    ## 5 factor(tc)2400        -0.0812    0.0221    -3.67  2.47e- 4
    ## 6 factor(race)black      0.0754    0.0388     1.94  5.22e- 2
    ## 7 factor(race)Hispanic   0.0393    0.0357     1.10  2.71e- 1
    ## 8 factor(race)other     -0.210     0.306     -0.686 4.93e- 1
    ## 9 factor(race)white      0.0778    0.0163     4.78  1.79e- 6

``` r
summary(model_with_race)
```

    ## 
    ## Call:
    ## lm(formula = exited ~ 1 + factor(gender) + factor(tc) + factor(race), 
    ##     data = training_set)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3407 -0.2901 -0.2296  0.6593  0.8690 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.21226    0.02170   9.782  < 2e-16 ***
    ## factor(gender)male    0.02073    0.01501   1.381 0.167286    
    ## factor(tc)1700       -0.07553    0.01989  -3.797 0.000149 ***
    ## factor(tc)2100        0.02991    0.01998   1.497 0.134435    
    ## factor(tc)2400       -0.08122    0.02214  -3.669 0.000247 ***
    ## factor(race)black     0.07541    0.03884   1.942 0.052231 .  
    ## factor(race)Hispanic  0.03933    0.03570   1.102 0.270668    
    ## factor(race)other    -0.21017    0.30631  -0.686 0.492651    
    ## factor(race)white     0.07781    0.01627   4.783 1.79e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4326 on 4120 degrees of freedom
    ##   (673 observations deleted due to missingness)
    ## Multiple R-squared:  0.01892,    Adjusted R-squared:  0.01701 
    ## F-statistic: 9.929 on 8 and 4120 DF,  p-value: 8.78e-14

``` r
#Check prediction against validation set
model_pred_2 <- predict(model_with_race, newdata = validation_set)
result_2 <- data.frame(actuals = validation_set$exited, predictions = model_pred_2)
result_2 <- result_2 %>%
  mutate(predictions = if_else(predictions >= 0.5, 1, 0))
#Check false negatives and false positives
confusion_matrix_2 <- table(result$predictions, result$actuals)
table_with_race<- prop.table(confusion_matrix_2["0", "1"])
```
