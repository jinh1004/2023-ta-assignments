Exercise 2 Part 4
================
Team 2
2023-01-24

# Import the Data

``` r
indiv = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/final_indiv.csv')
quarter = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/final_quarter.csv')

art_curr = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/current_art.csv')

art_people = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/people_art.csv')
art_gender = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/art_gender.csv')
art_race = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise2_data/art_race.csv')
```

# Turnover

There is a good balance between people who are still in the company and
people who have left

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
indiv %>% group_by(sep_ind) %>% summarize(sep = n())
```

    ## # A tibble: 2 × 2
    ##   sep_ind    sep
    ##   <lgl>    <int>
    ## 1 FALSE   148375
    ## 2 TRUE    106677

``` r
turnover = glm(sep_ind ~ gender + race + tenure, data = indiv, family = 'binomial')
```

    ## Warning: glm.fit: algorithm did not converge

``` r
summary(turnover)
```

    ## 
    ## Call:
    ## glm(formula = sep_ind ~ gender + race + tenure, family = "binomial", 
    ##     data = indiv)
    ## 
    ## Deviance Residuals: 
    ##        Min          1Q      Median          3Q         Max  
    ## -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06  -2.409e-06  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  -2.657e+01  5.399e+03  -0.005    0.996
    ## genderM       1.186e-12  3.070e+03   0.000    1.000
    ## raceBlack    -8.352e-12  9.505e+03   0.000    1.000
    ## raceHispanic -7.488e-12  4.844e+03   0.000    1.000
    ## raceOther    -8.329e-12  2.184e+04   0.000    1.000
    ## raceWhite    -7.923e-12  3.653e+03   0.000    1.000
    ## tenure       -1.199e-16  6.079e-01   0.000    1.000
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 0.0000e+00  on 130691  degrees of freedom
    ## Residual deviance: 7.5822e-07  on 130685  degrees of freedom
    ##   (124360 observations deleted due to missingness)
    ## AIC: 14
    ## 
    ## Number of Fisher Scoring iterations: 25

# Change in art unit (mobility)

The number of people who have changed art units far outweigh those who
did not, so we can expect a very high accuracy if we split the data and
predicted mobility on the test set. However, if we look at the confusion
matrix it will produce, the model will most likely falsely identify
FALSE cases as being TRUE

``` r
library(dplyr)
indiv %>% group_by(move_ind) %>% summarize(sep = n())
```

    ## # A tibble: 3 × 2
    ##   move_ind    sep
    ##   <lgl>     <int>
    ## 1 FALSE      6375
    ## 2 TRUE     142000
    ## 3 NA       106677

``` r
mobility = glm(move_ind ~ gender + race + tenure, data = indiv, family = 'binomial')
summary(mobility)
```

    ## 
    ## Call:
    ## glm(formula = move_ind ~ gender + race + tenure, family = "binomial", 
    ##     data = indiv)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.5982   0.0631   0.0631   0.3474   1.0406  
    ## 
    ## Coefficients:
    ##                Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   4.605e+00  6.678e-02  68.965  < 2e-16 ***
    ## genderM      -7.430e-01  3.782e-02 -19.647  < 2e-16 ***
    ## raceBlack    -2.261e+00  7.569e-02 -29.876  < 2e-16 ***
    ## raceHispanic  1.865e+00  1.064e-01  17.519  < 2e-16 ***
    ## raceOther    -1.282e+00  1.972e-01  -6.501 7.97e-11 ***
    ## raceWhite    -1.108e+00  4.933e-02 -22.457  < 2e-16 ***
    ## tenure       -1.180e-04  5.807e-06 -20.314  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 43785  on 130691  degrees of freedom
    ## Residual deviance: 35854  on 130685  degrees of freedom
    ##   (124360 observations deleted due to missingness)
    ## AIC: 35868
    ## 
    ## Number of Fisher Scoring iterations: 8
