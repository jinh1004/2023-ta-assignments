Project – 4. Linear Regression
================
Team 2
2023-02-16

# Imports

``` r
# Import application data
library(arrow)
app = read_parquet('C:/Users/jinho/Documents/2023-ta-assignments/project_data/app_data_with_gender.parquet')

# Import libraries
library(dplyr)
library(lubridate)

# Add quarter
app$quarter <- quarter(app$filing_date, type <- 'year.quarter')

# Import examiner and art unit level data created in parts 1 and 2
examiner = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/project_data/final_examiner.csv')
au = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/project_data/final_artunit.csv')
```

# Get Predictors

``` r
efficiency <- app %>% select(application_number, examiner_id, examiner_art_unit, quarter, tc)

examiner <- examiner %>% select(examiner_id, gender, race, tenure_days)
efficiency <- efficiency %>% left_join(examiner, by = 'examiner_id')

au <- au %>% select(quarter, examiner_art_unit, size, per_female, per_white, per_black, per_Asian, per_Hispanic)
efficiency <- efficiency %>% left_join(au, by = c('examiner_art_unit', 'quarter'))
efficiency
```

    ## # A tibble: 2,018,477 × 14
    ##    applicatio…¹ exami…² exami…³ quarter    tc gender race  tenur…⁴  size per_f…⁵
    ##    <chr>          <dbl>   <dbl>   <dbl> <dbl> <chr>  <chr>   <int> <int>   <dbl>
    ##  1 08284457       96082    1764   2000.  1700 Female White    5926   265   0.585
    ##  2 08413193       87678    1764   2000.  1700 <NA>   <NA>       NA   273   0.579
    ##  3 08531853       63213    1752   2000.  1700 Female White    6344   268   0.638
    ##  4 08637752       73788    1648   2001.  1600 Female White    6331   221   0.457
    ##  5 08682726       77294    1762   2000.  1700 Male   White    6332   298   0.419
    ##  6 08687412       68606    1734   2000.  1700 Female White    6345   187   0.358
    ##  7 08716371       89557    1627   2004.  1600 Female White    5634    55   0.4  
    ##  8 08765941       97543    1645   2000.  1600 Female White    7221   108   0.463
    ##  9 08776818       98714    1637   2000.  1600 Female White    6331    69   0.333
    ## 10 08809677       65530    1723   2002.  1700 Female Asian    6345   308   0.127
    ## # … with 2,018,467 more rows, 4 more variables: per_white <dbl>,
    ## #   per_black <dbl>, per_Asian <dbl>, per_Hispanic <dbl>, and abbreviated
    ## #   variable names ¹​application_number, ²​examiner_id, ³​examiner_art_unit,
    ## #   ⁴​tenure_days, ⁵​per_female

# Get Processing Time

Let processing time be the difference between the filing date and either
the date of abandonment, of issue, or of the latest statue update

``` r
processing_time <- app %>% select(application_number, filing_date, appl_status_date, patent_issue_date, abandon_date)
processing_time <- processing_time %>% mutate(filing_date = ymd(filing_date), appl_status_date = as_date(dmy_hms(appl_status_date)))
processing_time <- processing_time %>% mutate(end_time = coalesce(abandon_date, patent_issue_date))

processing_time$appl_status_date <- as.character(processing_time$appl_status_date)
processing_time$appl_status_date <- ifelse(!is.na(processing_time$end_time), NA, processing_time$appl_status_date)
processing_time$appl_status_date <- as.Date(processing_time$appl_status_date, "%Y-%m-%d")

processing_time <- processing_time %>% mutate(end_time = coalesce(end_time, appl_status_date))
processing_time <- processing_time %>% mutate(processing_time = end_time - filing_date)
```

# Clean Data

``` r
processing_time <- processing_time %>% select(application_number, processing_time)
efficiency <- efficiency %>% left_join(processing_time, by = 'application_number')

# Drop columns
efficiency <- efficiency %>% select(-application_number, -examiner_id, -examiner_art_unit, -quarter)

# Remove NAs
efficiency <- na.omit(efficiency)

# Remove negative processing times
efficiency <- efficiency[efficiency$processing_time >= 0,]
```

# Change types

``` r
efficiency$processing_time <- as.numeric(efficiency$processing_time)

efficiency$tc <- as.factor(efficiency$tc)
efficiency$gender <- as.factor(efficiency$gender)
efficiency$race <- as.factor(efficiency$race)
```

# Splitting the Data

``` r
library(caTools)

set.seed(119)

sample <- sample.split(efficiency$tc, SplitRatio = 0.8)
train <- subset(efficiency, sample == TRUE)
test <- subset(efficiency, sample == FALSE)
```

# Linear Regression

``` r
lr <- lm(processing_time ~ tc + gender + race + tenure_days + size + per_female + per_white + per_black + per_Asian + per_Hispanic, data = train)
summary(lr)
```

    ## 
    ## Call:
    ## lm(formula = processing_time ~ tc + gender + race + tenure_days + 
    ##     size + per_female + per_white + per_black + per_Asian + per_Hispanic, 
    ##     data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1529.1  -428.4   -94.0   322.3  5164.4 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)   2.880e+03  7.204e+01   39.979  < 2e-16 ***
    ## tc1700       -2.274e+01  1.688e+00  -13.468  < 2e-16 ***
    ## tc2100        1.453e+02  2.225e+00   65.313  < 2e-16 ***
    ## tc2400        1.786e+02  2.557e+00   69.848  < 2e-16 ***
    ## genderMale   -8.653e+00  1.516e+00   -5.707 1.15e-08 ***
    ## raceBlack    -1.701e+01  3.146e+00   -5.406 6.44e-08 ***
    ## raceHispanic  9.253e+01  4.483e+00   20.639  < 2e-16 ***
    ## raceOther     2.247e+02  1.126e+01   19.956  < 2e-16 ***
    ## raceWhite     5.592e+00  2.107e+00    2.654  0.00796 ** 
    ## tenure_days   2.714e-05  6.477e-06    4.190 2.79e-05 ***
    ## size         -3.857e-01  3.264e-03 -118.195  < 2e-16 ***
    ## per_female   -2.320e+01  4.417e+00   -5.252 1.50e-07 ***
    ## per_white    -1.688e+03  7.220e+01  -23.379  < 2e-16 ***
    ## per_black    -1.793e+03  7.243e+01  -24.757  < 2e-16 ***
    ## per_Asian    -1.753e+03  7.245e+01  -24.191  < 2e-16 ***
    ## per_Hispanic -1.992e+03  7.285e+01  -27.345  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 646.8 on 1004634 degrees of freedom
    ## Multiple R-squared:  0.03775,    Adjusted R-squared:  0.03774 
    ## F-statistic:  2628 on 15 and 1004634 DF,  p-value: < 2.2e-16

# Assess Performance

``` r
predictions <- predict(lr, data = test)

mse <- mean((predictions - train$processing_time)^2)
mse
```

    ## [1] 418332.9

``` r
rmse <- sqrt(mean((predictions - train$processing_time)^2))
rmse
```

    ## [1] 646.7866

# Export Data

``` r
write.csv(efficiency, 'C:/Users/jinho/Documents/2023-ta-assignments/project_data/final_efficiency.csv')
```
