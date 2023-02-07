Exercise 4 Part 2
================
Team 2
2023-02-07

# Import the Data

``` r
art_unit <- read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise4_data/art_unit_breakdown.csv')
```

# Average

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.2

``` r
# We want to look at the proportion of female and races over quarters
# As such, we should take the average proportion across al the art units
avg_per_quarter <- art_unit %>% group_by(quarter) %>% summarize(avg_female = mean(no_female), avg_white = mean(no_white), avg_black = mean(no_black), avg_asian = mean(no_Asian), avg_hispanic = mean(no_Hispanic))
names(avg_per_quarter)
```

    ## [1] "quarter"      "avg_female"   "avg_white"    "avg_black"    "avg_asian"   
    ## [6] "avg_hispanic"

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.2.2

    ## Loading required package: timechange

    ## Warning: package 'timechange' was built under R version 4.2.2

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.2.2

``` r
df1 <- avg_per_quarter %>% select(quarter, avg_female)
df2 <- avg_per_quarter %>% select(quarter, avg_white)
df3 <- avg_per_quarter %>% select(quarter, avg_black)
df4 <- avg_per_quarter %>% select(quarter, avg_asian)
df5 <- avg_per_quarter %>% select(quarter, avg_hispanic)

colnames(df1) <- c('quarter', 'avg')
colnames(df2) <- c('quarter', 'avg')
colnames(df3) <- c('quarter', 'avg')
colnames(df4) <- c('quarter', 'avg')
colnames(df5) <- c('quarter', 'avg')

# Bind the dataframes above into a one for the sake of visualizing all of them on the same graph
df <- rbind(df1, df2, df3, df4, df5)
rm(avg_per_quarter, df1, df2, df3, df4, df5)

# Add a column to indicate which group record belongs to
df$group <- c(rep('avg_female', 70), rep('avg_white', 70), rep('avg_black', 70),
              rep('avg_asian', 70), rep('avg_hispanic', 70))

# Plot the graph
ggplot(df, aes(quarter, avg, color = group)) + geom_line() + ggtitle('Average Art Unit Demographic') + xlab('Quarter') + ylab('Percentage')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# For the sake of analysis, let us take a look of quarters starting 2015
df_2015 <- df[df$quarter >= 2015.1,]
ggplot(df_2015, aes(quarter, avg, color = group)) + geom_line() + ggtitle('Average Art Unit Demographic') + xlab('Quarter') + ylab('Percentage')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Mobility Rate

``` r
# Import mobility data that was created in part 1
mobility <- read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise4_data/mobility.csv')
```

``` r
# calculate the mobility rate
mobility_rate <- mobility %>% select(quarter, move_ind)
mobility_rate <- na.omit(mobility_rate)
mobility_rate <- mobility_rate %>% group_by(quarter) %>% summarize(total = n(), moved = sum(move_ind), move_rate = moved/total)

# Plot mobility rate
ggplot(mobility_rate, aes(quarter, move_rate)) + geom_line() + ggtitle('Mobility Rate') + xlab('Quarter') + ylab('Mobility Rate')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# For the sake of analysis, let us take a look of quarters starting 2015
mobility_rate_2015 <- mobility_rate[mobility_rate$quarter >= 2015.1,]
ggplot(mobility_rate_2015, aes(quarter, move_rate)) + geom_line() + ggtitle('Mobility Rate') + xlab('Quarter') + ylab('Mobility Rate')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Combine the above Into a Single Graph

Due to the scale of the variables, we are unable to see the change of
the proportions and the change in the mobility rate at the same time.
The best method to compare these graphs is thus just to align the y-axis
of the graphs above and analyze the trend.

``` r
mobility_rate1 <- mobility_rate %>% select(quarter, move_rate)
colnames(mobility_rate1) <- c('quarter', 'avg')
mobility_rate1$group <- NA

df_1 <- rbind(df, mobility_rate1)
df_1$group <- c(rep('avg_female', 70), rep('avg_white', 70), rep('avg_black', 70),
             rep('avg_asian', 70), rep('avg_hispanic', 70), rep('mobility_rate', 70))

ggplot(df_1, aes(quarter, avg, color = group)) + geom_line() + ggtitle('Average Art Unit Demographic') + xlab('Quarter') + ylab('Percentage')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
df_2015 <- df_1[df_1$quarter >= 2015.1,]
ggplot(df_2015, aes(quarter, avg, color = group)) + geom_line() + ggtitle('Average Art Unit Demographic') + xlab('Quarter') + ylab('Percentage')
```

![](exercise4_part2_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
