Exercise 3 Part 1
================
Team 2
2023-01-24

# Import the Data

``` r
library(arrow)
app = read_parquet('C:/Users/jinho/Documents/2023-ta-assignments/exercise3_data/app_data_sample.parquet')
```

``` r
library(lubridate)
library(dplyr)

person_level_data <- app %>% 
  group_by(examiner_id) %>% 
  summarise(
    art_unit = min(examiner_art_unit, na.rm = TRUE),
    gender = min(gender, na.rm = TRUE),
    start_year = min(year(earliest_date), na.rm = TRUE),
    latest_date = max(latest_date, na.rm = TRUE),
    tenure_days = max(tenure_days, na.rm = TRUE)
  ) %>% 
  mutate(
    tc = floor(art_unit/100)*100,
    work_group = floor(art_unit/10)*10
  ) %>% 
  filter(!is.na(gender)) # dropping all records where we don't know the gender

person_level_data
```

    ## # A tibble: 4,849 × 8
    ##    examiner_id art_unit gender start_year latest_date tenure_days    tc work_g…¹
    ##          <dbl>    <dbl> <chr>       <dbl> <date>            <dbl> <dbl>    <dbl>
    ##  1       59012     1716 male         2004 2015-07-24         4013  1700     1710
    ##  2       59025     2465 male         2009 2017-05-18         2761  2400     2460
    ##  3       59040     1724 female       2007 2017-05-23         3542  1700     1720
    ##  4       59052     2138 male         2001 2007-02-28         2017  2100     2130
    ##  5       59055     2165 male         2004 2007-12-26         1149  2100     2160
    ##  6       59056     2124 male         2000 2017-05-22         6268  2100     2120
    ##  7       59081     2489 male         2011 2017-05-19         2220  2400     2480
    ##  8       59086     2487 female       2010 2017-05-18         2527  2400     2480
    ##  9       59096     1612 male         2000 2015-11-20         5800  1600     1610
    ## 10       59117     2439 male         2009 2011-09-02          925  2400     2430
    ## # … with 4,839 more rows, and abbreviated variable name ¹​work_group

``` r
person_level_data <- person_level_data[, c('examiner_id', 'work_group')]
app <- app %>% left_join(person_level_data, by = 'examiner_id')

rm(person_level_data)
```

# Art Unit Mobility

``` r
mobility_art <- app[,c('examiner_id', 'filing_date', 'examiner_art_unit')]
mobility_tc <- app[,c('examiner_id', 'filing_date', 'tc')]
mobility_wg <- app[,c('examiner_id', 'filing_date', 'work_group')]

mobility_art <- mobility_art %>% group_by(examiner_id, examiner_art_unit) %>% summarize(start = min(filing_date), end = max(filing_date), time_in_art = end - start)

mobility_tc <- mobility_tc %>% group_by(examiner_id, tc) %>% summarize(start = min(filing_date), end = max(filing_date), time_in_tc = end - start)

mobility_wg <- mobility_wg %>% group_by(examiner_id, work_group) %>% summarize(start = min(filing_date), end = max(filing_date), time_in_wg = end - start)


mobility_art <- distinct(mobility_art[, c('examiner_id', 'examiner_art_unit', 'time_in_art')])
mobility_tc <- distinct(mobility_tc[, c('examiner_id', 'tc', 'time_in_tc')])
mobility_wg <- distinct(mobility_wg[, c('examiner_id', 'work_group', 'time_in_wg')])

app <- app %>% left_join(mobility_art, by = c('examiner_id', 'examiner_art_unit'))
app <- app %>% left_join(mobility_tc, by = c('examiner_id', 'tc'))
app <- app %>% left_join(mobility_wg, by = c('examiner_id', 'work_group'))
```

``` r
mobility_art <- mobility_art %>% group_by(examiner_id) %>% summarize(avg_art = mean(time_in_art))
mobility_tc <- mobility_tc %>% group_by(examiner_id) %>% summarize(avg_tc = mean(time_in_tc))
mobility_wg <- mobility_wg %>% group_by(examiner_id) %>% summarize(avg_wg = mean(time_in_wg))

rm(mobility_art, mobility_tc, mobility_wg)
```

# Rate of Processing Applications

``` r
rate <- app %>% group_by(examiner_id) %>% summarize(count = n(), tenure = tenure_days, rate = n()/tenure_days)
rate <- distinct(rate[, c('examiner_id', 'rate')])
app <- app %>% left_join(rate, by = 'examiner_id')
rm(rate)
```

# Mobility

``` r
mobility <- app[, c('examiner_id', 'examiner_art_unit')]

mobility <- mobility %>% group_by(examiner_id) %>% summarize(count = n_distinct(examiner_art_unit))

mobility$move_ind <- case_when(
  mobility$count == 1 ~ 0,
  TRUE ~ 1
)

table(mobility$move_ind)
```

    ## 
    ##    0    1 
    ## 2886 2763

``` r
mobility <- mobility[, c('examiner_id', 'move_ind')]
app <- app %>% left_join(mobility, by = c('examiner_id'))
rm(mobility)
```

# Transform Data

Drop columns

``` r
names(app)
```

    ##  [1] "application_number"   "filing_date"          "examiner_name_last"  
    ##  [4] "examiner_name_first"  "examiner_name_middle" "examiner_id"         
    ##  [7] "examiner_art_unit"    "uspc_class"           "uspc_subclass"       
    ## [10] "patent_number"        "patent_issue_date"    "abandon_date"        
    ## [13] "disposal_type"        "appl_status_code"     "appl_status_date"    
    ## [16] "tc"                   "gender"               "race"                
    ## [19] "earliest_date"        "latest_date"          "tenure_days"         
    ## [22] "work_group"           "time_in_art"          "time_in_tc"          
    ## [25] "time_in_wg"           "rate"                 "move_ind"

``` r
app <- app[,c('gender','race','tenure_days','tc','time_in_art','time_in_tc','time_in_wg','rate','move_ind')]
```

Remove NAs

``` r
app <- na.omit(app)
```

Keep Distinct

``` r
app <- distinct(app)
```

# Export Data

``` r
# write.csv(app, 'C:/Users/jinho/Documents/2023-ta-assignments/exercise3_data/app.csv', row.names = FALSE)
```
