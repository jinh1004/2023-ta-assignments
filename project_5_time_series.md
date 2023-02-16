Project – 5. Time Series Analysis
================
Team 2
2023-02-16

# Imports

``` r
# Import application data
library(arrow)
app = read_parquet('C:/Users/jinho/Documents/2023-ta-assignments/project_data/app_data_sample_with_race.parquet')

# Import libraries
library(dplyr)
library(lubridate)
library(caTools)
```

# Pre-Processing

## Get Processing Time

``` r
no_production <- app %>% select(application_number, filing_date, appl_status_date, patent_issue_date, abandon_date)
no_production <- no_production %>% mutate(filing_date = ymd(filing_date), appl_status_date = as_date(dmy_hms(appl_status_date)))
no_production <- no_production %>% mutate(end_time = coalesce(abandon_date, patent_issue_date))

no_production$appl_status_date <- as.character(no_production$appl_status_date)
no_production$appl_status_date <- ifelse(!is.na(no_production$end_time), NA, no_production$appl_status_date)
no_production$appl_status_date <- as.Date(no_production$appl_status_date, "%Y-%m-%d")

no_production <- no_production %>% mutate(end_time = coalesce(end_time, appl_status_date))
no_production <- no_production %>% mutate(processing_time = end_time - filing_date)
no_production <- no_production %>% select(application_number, processing_time)
```

## Clean

``` r
app <- app %>% left_join(no_production, by = 'application_number')
time_series <- app %>% select(filing_date, processing_time, gender, tc)
time_series <- time_series[time_series$processing_time >= 0,]
```

# Overall

``` r
ts_overall <- time_series %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))
colnames(ts_overall) <- c('ds', 'y')
```

## Time Series

``` r
library(prophet)
```

    ## Warning: package 'prophet' was built under R version 4.2.2

    ## Warning: package 'rlang' was built under R version 4.2.2

``` r
overall <- prophet(ts_overall, growth = 'linear')
overall_future <- make_future_dataframe(overall, periods = 365)
overall_future <- predict(overall, overall_future)
plot(overall, overall_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
prophet_plot_components(overall, overall_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Gender

``` r
ts_female <- time_series %>% filter(gender == 'female') %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))
ts_male <- time_series %>% filter(gender == 'male') %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))

colnames(ts_female) <- c('ds', 'y')
colnames(ts_male) <- c('ds', 'y')
```

## Time Series

``` r
female <- prophet(ts_female, growth = 'linear')
female_future <- make_future_dataframe(female, periods = 365)
female_future <- predict(female, female_future)

male <- prophet(ts_male, growth = 'linear')
male_future <- make_future_dataframe(male, periods = 365)
male_future <- predict(male, male_future)
```

``` r
plot(female, female_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(male, male_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
prophet_plot_components(female, female_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
prophet_plot_components(male, male_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

# Technology Center

``` r
ts_1600 <- time_series %>% filter(tc == 1600) %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))
ts_1700 <- time_series %>% filter(tc == 1700) %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))
ts_2100 <- time_series %>% filter(tc == 2100) %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))
ts_2400 <- time_series %>% filter(tc == 2400) %>% group_by(filing_date) %>% summarize(avg_processing = as.numeric(mean(processing_time)))

colnames(ts_1600) <- c('ds', 'y')
colnames(ts_1700) <- c('ds', 'y')
colnames(ts_2100) <- c('ds', 'y')
colnames(ts_2400) <- c('ds', 'y')
```

## Time Series

``` r
tc1600 <- prophet(ts_1600, growth = 'linear')
tc1600_future <- make_future_dataframe(tc1600, periods = 365)
tc1600_future <- predict(tc1600, tc1600_future)

tc1700 <- prophet(ts_1700, growth = 'linear')
tc1700_future <- make_future_dataframe(tc1700, periods = 365)
tc1700_future <- predict(tc1600, tc1700_future)

tc2100 <- prophet(ts_2100, growth = 'linear')
tc2100_future <- make_future_dataframe(tc2100, periods = 365)
tc2100_future <- predict(tc1600, tc2100_future)

tc2400 <- prophet(ts_2400, growth = 'linear')
tc2400_future <- make_future_dataframe(tc2400, periods = 365)
tc2400_future <- predict(tc1600, tc2400_future)
```

``` r
plot(tc1600, tc1600_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
plot(tc1700, tc1700_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
plot(tc2100, tc2100_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

``` r
plot(tc2400, tc2400_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-13-4.png)<!-- -->

``` r
prophet_plot_components(tc1600, tc1600_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
prophet_plot_components(tc1700, tc1700_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
prophet_plot_components(tc2100, tc2100_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->

``` r
prophet_plot_components(tc2400, tc2400_future)
```

![](project_5_time_series_files/figure-gfm/unnamed-chunk-14-4.png)<!-- -->
