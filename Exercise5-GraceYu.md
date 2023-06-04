Exercise 5 - Grace Yu
================

``` r
data_path <- "F:/Dropbox/People Analytics/"
data <- read_parquet(paste0(data_path,"filtered_data.parquet"))
data1600 <- filter(data, tc==1600)
data1700 <- filter(data, tc==1700)
data2100 <- filter(data, tc==2100)
data2400 <- filter(data, tc==2400)
```

``` r
#employees are examiners, need to group into one

data1600 <- data1600 %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
ex4_data1600 <- data1600 %>%
  group_by(examiner_id) %>%
  summarize(tc = first(tc), wg = floor(first(examiner_art_unit)/10)*10 , au= first(examiner_art_unit), gender = first(gender))

data1700 <- data1700 %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
ex4_data1700 <- data1700 %>%
  group_by(examiner_id) %>%
  summarize(tc = first(tc), wg = floor(first(examiner_art_unit)/10)*10 , au= first(examiner_art_unit), gender = first(gender))

data2100 <- data2100 %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
ex4_data2100 <- data2100 %>%
  group_by(examiner_id) %>%
  summarize(tc = first(tc), wg = floor(first(examiner_art_unit)/10)*10 , au= first(examiner_art_unit), gender = first(gender))

data2400 <- data2400 %>%
  mutate(earliest_date = year(earliest_date), latest_date = year(latest_date))
ex4_data2400 <- data2400 %>%
  group_by(examiner_id) %>%
  summarize(tc = first(tc), wg = floor(first(examiner_art_unit)/10)*10 , au= first(examiner_art_unit), gender = first(gender))
```

``` r
barplot(table(ex4_data1600$gender), main = "TC 1600")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-1.png)<!-- -->

``` r
barplot(table(ex4_data1700$gender), main = "TC 1700")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-2.png)<!-- -->

``` r
barplot(table(ex4_data2100$gender), main = "TC 2100")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-3.png)<!-- -->

``` r
barplot(table(ex4_data2400$gender), main = "TC 2400")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-4.png)<!-- -->

``` r
pie(table(ex4_data1600$gender), main = "TC 1600")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-5.png)<!-- -->

``` r
pie(table(ex4_data1700$gender), main = "TC 1700")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-6.png)<!-- -->

``` r
pie(table(ex4_data2100$gender), main = "TC 2100")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-7.png)<!-- -->

``` r
pie(table(ex4_data2400$gender), main = "TC 2400")
```

![](Exercise5-GraceYu_files/figure-gfm/exercise5-8.png)<!-- -->

``` r
temp <- filter(ex4_data1700, au== 1797)
temp <- temp %>% drop_na()
barplot(table(temp$gender))
```

![](Exercise5-GraceYu_files/figure-gfm/graph-1.png)<!-- -->
