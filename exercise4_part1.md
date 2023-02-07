Exercise 4 Part 1
================
Team 2
2023-02-07

# Import the Data

``` r
library(arrow)
app <- read_parquet('C:/Users/jinho/Documents/2023-ta-assignments/exercise4_data/app_data_sample.parquet')
names(app)
```

    ##  [1] "application_number"   "filing_date"          "examiner_name_last"  
    ##  [4] "examiner_name_first"  "examiner_name_middle" "examiner_id"         
    ##  [7] "examiner_art_unit"    "uspc_class"           "uspc_subclass"       
    ## [10] "patent_number"        "patent_issue_date"    "abandon_date"        
    ## [13] "disposal_type"        "appl_status_code"     "appl_status_date"    
    ## [16] "tc"                   "gender"               "race"                
    ## [19] "earliest_date"        "latest_date"          "tenure_days"

# Pre-processing

``` r
library(dplyr)

# Drop columns that we will not need
app <- select(app, -c('examiner_name_last', 'examiner_name_first', 'examiner_name_middle', 'uspc_class', 'uspc_subclass', 'patent_number', 'patent_issue_date', 'abandon_date', 'disposal_type', 'appl_status_code', 'appl_status_date'))
names(app)
```

    ##  [1] "application_number" "filing_date"        "examiner_id"       
    ##  [4] "examiner_art_unit"  "tc"                 "gender"            
    ##  [7] "race"               "earliest_date"      "latest_date"       
    ## [10] "tenure_days"

``` r
# Drop NAs
app <- na.omit(app)
```

# Add quarter

``` r
library(lubridate)
# Add quarter from filing_date
app$quarter <- quarter(app$filing_date, type <- 'year.quarter')
```

# Size of art Unit

``` r
app1 <- app %>% group_by(quarter, examiner_art_unit) %>% summarize(size = n())
```

# Percentage of female

``` r
gender_ratio <- app %>% filter(gender == 'female') %>% group_by(quarter, examiner_art_unit) %>% summarize(no_female = n())
app1 <- app1 %>% left_join(gender_ratio, by <- c('quarter', 'examiner_art_unit'))
rm(gender_ratio)

# Replace all the NAs with 0
app1$no_female <- ifelse(is.na(app1$no_female), 0, app1$no_female)
app1$Per_female <- app1$no_female / app1$size
```

# Percentage of race

``` r
race_list <- c('white', 'black', 'Asian', 'Hispanic')

# Add the number of white, black, Asian, and Hispanic to app1
for (i in race_list) {
  race_ratio <- app %>% filter(race == i) %>% group_by(quarter, examiner_art_unit) %>% summarize(temp = n())
  app1 <- app1 %>% left_join(race_ratio, by <- c('quarter', 'examiner_art_unit'))
}

rm(race_ratio)

# Change the column names of the numbers created above
for (i in 1:4) {
  colnames(app1)[(i + 5)] <- paste('no_', race_list[i], sep = '')
}

# Replace all NAs with 0
app1 <- app1 %>% mutate_at(vars(no_white, no_black, no_Asian,  no_Hispanic), ~ replace(., is.na(.), 0))

app1$per_white <- app1$no_white / app1$size
app1$per_black <- app1$no_black / app1$size
app1$per_Asian <- app1$no_Asian / app1$size
app1$per_Hispanic <- app1$no_Hispanic / app1$size
```

# Mobility per Quarter

``` r
mobility <- app %>% select(examiner_id, examiner_art_unit, quarter, filing_date)

# Since an examiner may have multiple filing_dates in a quarter, take the most recent one
mobility1 <- app %>% group_by(examiner_id, quarter) %>% summarize(filing_date = max(filing_date))

mobility <- mobility1 %>% left_join(mobility, by <- c('examiner_id', 'quarter', 'filing_date'))

rm(mobility1)

mobility <- distinct(mobility)
mobility <- mobility %>% mutate(move_ind = NA)
```

``` r
# The mobility table is sorted by examiner and then by filing_date
# So if the examiner_id and examiner_art_unit of the row below is the same as the one above, it means that the examiner did not move art unit
# If the examiner_id is the same, but the examiner_art_unit is different between the row above and below, then it means that the examiner moved art unit
# If the examiner_id in the row above and below are different, then we should fill the cell with NA as it would not make sense to compare these two rows
for (i in 2:nrow(mobility)) {
  if ((mobility$examiner_id[i] == mobility$examiner_id[i-1]) &&
      (mobility$examiner_art_unit[i] == mobility$examiner_art_unit[i-1])) {
    mobility$move_ind[i] = 0
  } else if ((mobility$examiner_id[i] == mobility$examiner_id[i-1]) &&
      (mobility$examiner_art_unit[i] != mobility$examiner_art_unit[i-1])) {
    mobility$move_ind[i] = 1
  }
}
```

# Export Data

``` r
# write.csv(mobility, 'C:/Users/jinho/Documents/2023-ta-assignments/exercise4_data/mobility.csv', row.names = FALSE)
# write.csv(app1, 'C:/Users/jinho/Documents/2023-ta-assignments/exercise4_data/art_unit_breakdown.csv', row.names = FALSE)
```
