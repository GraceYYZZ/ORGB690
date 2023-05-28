Exercise 4 - Grace Yu
================

``` r
data_path <- "F:/Dropbox/People Analytics/"
data <- read_parquet(paste0(data_path,"filtered_data.parquet"))
data1700 <- filter(data, tc==1700)
```

``` r
#employees are examiners, need to group into one
data1700 <- data1700 %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
ex4_data1700 <- data1700 %>%
  group_by(examiner_id) %>%
  summarize(tc = first(tc), wg = floor(first(examiner_art_unit)/10)*10 , au= first(examiner_art_unit), gender = first(gender))
```

``` r
barplot(table(ex4_data1700$gender))
```

![](Exercise4-GraceYu_files/figure-gfm/exercise4-1.png)<!-- -->

``` r
barplot(table(ex4_data1700$gender,ex4_data1700$wg))
```

![](Exercise4-GraceYu_files/figure-gfm/exercise4-2.png)<!-- -->

``` r
barplot(table(ex4_data1700$gender,ex4_data1700$au))
```

![](Exercise4-GraceYu_files/figure-gfm/exercise4-3.png)<!-- -->

``` r
temp <- filter(ex4_data1700, au== 1797)
temp <- temp %>% drop_na()
barplot(table(temp$gender))
```

![](Exercise4-GraceYu_files/figure-gfm/graph-1.png)<!-- -->

``` r
#From every male's perspective, the distribution of gender in the AU is the same as the overall distribution, except with 1 less male. Same for every female. Difference is minimal when the AU has a lot of people, because removing 1 person barely affects the distribution of gender. Difference in perception may be greater when the AU is small (say 3 people: 2 male 1 female), which is reflected in the bar graph above.
```
