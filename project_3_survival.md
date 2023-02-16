Project – 3. Survival Analysis
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
```

# Application Data

We can think of abandoned applications as being “Dead” while issued and
pending patents as being “Censored”

``` r
df_app <- app %>% select(application_number, filing_date, examiner_art_unit, patent_issue_date, abandon_date, disposal_type, appl_status_date)
df_app$status <- ifelse(df_app$disposal_type == 'ABN', 2, 1)
```

The time should be the abandoned_date for applications that have been
abandoned, patent_issue_date for patents that have been issued, and the
latest status update date for the remaining pending applications

``` r
df_app <- df_app %>% mutate(time = coalesce(abandon_date, patent_issue_date)) %>% select(application_number, filing_date, time, status)

df_app$filing_date <- as.character(df_app$filing_date)
df_app$filing_date <- ifelse(!is.na(df_app$time), NA, df_app$filing_date)
df_app$filing_date <- as.Date(df_app$filing_date, "%Y-%m-%d")

df_app <- df_app %>% mutate(time = coalesce(time, filing_date)) %>% select(application_number, time, status)
df_app
```

    ## # A tibble: 2,018,477 × 3
    ##    application_number time       status
    ##    <chr>              <date>      <dbl>
    ##  1 08284457           2003-02-18      1
    ##  2 08413193           2002-08-27      1
    ##  3 08531853           1997-03-04      1
    ##  4 08637752           2005-08-09      1
    ##  5 08682726           2000-12-27      2
    ##  6 08687412           2001-07-31      1
    ##  7 08716371           2004-01-26      1
    ##  8 08765941           2001-08-22      2
    ##  9 08776818           2002-07-15      2
    ## 10 08809677           2005-02-22      1
    ## # … with 2,018,467 more rows

# Survival Analysis

``` r
library(survival)
```

    ## Warning: package 'survival' was built under R version 4.2.2

``` r
Survival_Function = survfit(Surv(as.numeric(time), status == 2) ~ 1, data = df_app)
Survival_Function
```

    ## Call: survfit(formula = Surv(as.numeric(time), status == 2) ~ 1, data = df_app)
    ## 
    ##            n events median 0.95LCL 0.95UCL
    ## [1,] 2018477 601411  17080   17078   17081

``` r
plot(Survival_Function, xlab = 'Time', ylab = 'Survival Rate')
```

![](project_3_survival_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
