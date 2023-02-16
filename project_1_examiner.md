Project – 1. Examiner Level Data
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
```

# Gender

Because I was unable to utilize the gender package on the current
version of R Studio, I have asked my teammate to utilize the package and
get the likelihood on the basis of first name that an examiner is
female.

``` r
# If gender is close to 0 --> 'Male'. If closer to 1 --> Female
df_gender <- app %>% select(examiner_id, gender)
df_gender$gender <- round(df_gender$gender, digits = 0)
df_gender <- transform(df_gender, gender = ifelse(gender == 1, 'Female', 'Male'))

# Keep distinct values to reduce processing time
df_gender <- distinct(df_gender)

# Drop NAs as there are many first names the gender package does not recognize
df_gender <- na.omit(df_gender)
```

# Race

``` r
library(wru)
```

    ## Warning: package 'wru' was built under R version 4.2.2

``` r
df_race <- app %>% select(examiner_id, examiner_name_first)
colnames(df_race) <- c('examiner_id', 'surname')

# Predict probability that an examiner is Asian, black, Hispanic, white, or other
df_race <- predict_race(voter.file = df_race, surname.only = TRUE)
```

    ## Warning: Unknown or uninitialised column: `state`.

    ## Proceeding with last name predictions...

    ## ℹ All local files already up-to-date!

    ## 447278 (22.2%) individuals' last names were not matched.

``` r
# Let the highest probability be the race of the examiner
df_race <- df_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ 'Asian',
    max_race_p == pred.bla ~ 'Black',
    max_race_p == pred.his ~ 'Hispanic',
    max_race_p == pred.oth ~ 'Other',
    max_race_p == pred.whi ~ 'White',
    TRUE ~ NA_character_
  ))

df_race <- df_race %>% select(examiner_id, race)
df_race <- distinct(df_race)
```

# Tenure

Let tenure be the difference between the earliest filing date of the
application an examiner is in charge of processing and the latest status
update date.

``` r
library(lubridate)
df_tenure <- app %>% select(examiner_id, filing_date, appl_status_date)

df_tenure <- df_tenure %>% mutate(start_date = ymd(filing_date),
                                  end_date = as_date(dmy_hms(appl_status_date)))

df_tenure <- df_tenure %>% group_by(examiner_id) %>% summarize(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )

df_tenure <- df_tenure %>% select(examiner_id, tenure_days)
```

# Turnover

Let’s assume that if an examiner has not been assigned a new patent
application in 180 days, they have left the company

``` r
df_turnover <- app %>% select(examiner_id, filing_date)

# Let the current date be the latest filing_date
df_turnover$current_date <- max(ymd(app$filing_date))

df_turnover <- df_turnover %>% group_by(examiner_id) %>% summarize(
  latest_date = max(filing_date, na.rm = TRUE),
  diff = current_date - latest_date
)

df_turnover <- transform(df_turnover, turnover_ind = ifelse(diff >= 180, 1, 0))

df_turnover1 <- df_turnover %>% select(examiner_id, turnover_ind)
df_turnover1 <- distinct(df_turnover1)
```

# Moving Art Unit

``` r
df_move <- app %>% select(examiner_id, examiner_art_unit)
df_move <- distinct(df_move)
df_move <- df_move %>% group_by(examiner_id) %>% summarize(no_au = n(),
                                                           move_ind = ifelse
                                                           (no_au == 1, 0, 1))
df_move <- df_move %>% select(examiner_id, move_ind)
```

# Combine

``` r
df_examiner <- df_gender
df_examiner <- df_examiner %>% left_join(df_race, id = 'examiner_id')
df_examiner <- df_examiner %>% left_join(df_tenure, id = 'examiner_id')
df_examiner <- df_examiner %>% left_join(df_turnover1, id = 'examiner_id')
df_examiner <- df_examiner %>% left_join(df_move, id = 'examiner_id')
```

# Export Data

``` r
write.csv(df_examiner, 'C:/Users/jinho/Documents/2023-ta-assignments/project_data/final_examiner.csv')
```
