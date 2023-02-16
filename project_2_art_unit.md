Project – 2. Art Unit Level Data
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
```

# Size of art Unit

``` r
# Add quarter from filing_date
app$quarter <- quarter(app$filing_date, type <- 'year.quarter')
df_au <- app %>% group_by(quarter, examiner_art_unit) %>% summarize(size = n())
```

# Percentage of female

``` r
gender_ratio <- app %>% filter(gender == 'female') %>% group_by(quarter, examiner_art_unit) %>% summarize(no_female = n())
df_au <- df_au %>% left_join(gender_ratio, by <- c('quarter', 'examiner_art_unit'))
rm(gender_ratio)

# Replace all the NAs with 0
df_au$no_female <- ifelse(is.na(df_au$no_female), 0, df_au$no_female)
df_au$per_female <- df_au$no_female / df_au$size
```

# Percentage of race

``` r
race_list <- c('white', 'black', 'Asian', 'Hispanic')

# Add the number of white, black, Asian, and Hispanic to df_au
for (i in race_list) {
  race_ratio <- app %>% filter(race == i) %>% group_by(quarter, examiner_art_unit) %>% summarize(temp = n())
  df_au <- df_au %>% left_join(race_ratio, by <- c('quarter', 'examiner_art_unit'))
}

rm(race_ratio)

# Change the column names of the numbers created above
for (i in 1:4) {
  colnames(df_au)[(i + 5)] <- paste('no_', race_list[i], sep = '')
}

# Replace all NAs with 0
df_au <- df_au %>% mutate_at(vars(no_white, no_black, no_Asian,  no_Hispanic), ~ replace(., is.na(.), 0))

df_au$per_white <- df_au$no_white / df_au$size
df_au$per_black <- df_au$no_black / df_au$size
df_au$per_Asian <- df_au$no_Asian / df_au$size
df_au$per_Hispanic <- df_au$no_Hispanic / df_au$size
```

# Number of applications

``` r
no_app <- app %>% group_by(quarter, examiner_art_unit) %>% summarize(no_app = n())
df_au <- df_au %>% left_join(no_app, by = c('quarter', 'examiner_art_unit'))
```

# Export Data

``` r
write.csv(df_au, 'C:/Users/jinho/Documents/2023-ta-assignments/project_data/final_artunit.csv')
```
