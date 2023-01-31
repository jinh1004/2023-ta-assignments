Exercise 3 Part 2
================
Team 2
2023-01-24

# Import data

``` r
app = read.csv('C:/Users/jinho/Documents/2023-ta-assignments/exercise3_data/app.csv')
```

Categorical

``` r
sapply(app, class)
```

    ##      gender        race tenure_days          tc time_in_art  time_in_tc 
    ## "character" "character"   "integer"   "integer"   "integer"   "integer" 
    ##  time_in_wg        rate    move_ind 
    ##   "integer"   "numeric"   "integer"

``` r
app$gender <- as.factor(app$gender)
app$race <- as.factor(app$race)
app$tc <- as.factor(app$tc)

app$move_ind <- as.factor(app$move_ind)
```

# Logistic Regression

``` r
library(bestglm)
```

    ## Loading required package: leaps

``` r
best_aic <- bestglm(Xy = app, family = binomial, IC = 'AIC', method = 'exhaustive')
```

    ## Morgan-Tatar search since family is non-gaussian.

    ## Note: factors present with more than 2 levels.

``` r
library(bestglm)
best_bic <- bestglm(Xy = app, family = binomial, IC = 'BIC', method = 'exhaustive')
```

    ## Morgan-Tatar search since family is non-gaussian.

    ## Note: factors present with more than 2 levels.

``` r
summary(best_aic$BestModel)
```

    ## 
    ## Call:
    ## glm(formula = y ~ ., family = family, data = Xi, weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1543  -0.2856   0.0000   0.0000   2.8899  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.710e+00  2.299e-01 -20.484  < 2e-16 ***
    ## tenure_days  3.203e-04  4.084e-05   7.842 4.43e-15 ***
    ## tc1700       1.324e+00  1.520e-01   8.709  < 2e-16 ***
    ## tc2100       1.089e+00  1.529e-01   7.123 1.06e-12 ***
    ## tc2400       1.083e+00  1.555e-01   6.967 3.24e-12 ***
    ## time_in_art -1.053e+01  1.275e+01  -0.826 0.409015    
    ## time_in_wg   1.053e+01  1.275e+01   0.826 0.409003    
    ## rate         5.367e+00  1.424e+00   3.770 0.000163 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10498.1  on 8808  degrees of freedom
    ## Residual deviance:  2832.4  on 8801  degrees of freedom
    ## AIC: 2848.4
    ## 
    ## Number of Fisher Scoring iterations: 25

``` r
summary(best_bic$BestModel)
```

    ## 
    ## Call:
    ## glm(formula = y ~ ., family = family, data = Xi, weights = weights)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.1543  -0.2856   0.0000   0.0000   2.8899  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.710e+00  2.299e-01 -20.484  < 2e-16 ***
    ## tenure_days  3.203e-04  4.084e-05   7.842 4.43e-15 ***
    ## tc1700       1.324e+00  1.520e-01   8.709  < 2e-16 ***
    ## tc2100       1.089e+00  1.529e-01   7.123 1.06e-12 ***
    ## tc2400       1.083e+00  1.555e-01   6.967 3.24e-12 ***
    ## time_in_art -1.053e+01  1.275e+01  -0.826 0.409015    
    ## time_in_wg   1.053e+01  1.275e+01   0.826 0.409003    
    ## rate         5.367e+00  1.424e+00   3.770 0.000163 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 10498.1  on 8808  degrees of freedom
    ## Residual deviance:  2832.4  on 8801  degrees of freedom
    ## AIC: 2848.4
    ## 
    ## Number of Fisher Scoring iterations: 25

``` r
library(lavaan)
```

    ## This is lavaan 0.6-13
    ## lavaan is FREE software! Please report any bugs.

``` r
BIC(best_aic$BestModel)
```

    ## [1] 2905.084

``` r
rm(best_aic, best_bic)
```

# Confusion matrix

``` r
app1 <- app[,c('move_ind', 'tenure_days', 'tc', 'time_in_art', 'time_in_wg', 'rate')]
```

``` r
library(caret)

set.seed(119)

i <- createDataPartition(app1$move_ind, p = 0.7, list = FALSE)
train1 <- app1[i, ]
test1 <- app1[-i, ]

model1 <- train(move_ind ~ tenure_days + tc + time_in_art + time_in_wg + rate, data = train1, method = 'glm', family = binomial)
pred1 <- predict(model1, test1, type = 'raw')

conf_matrix1 <- confusionMatrix(test1$move_ind, pred1)
conf_matrix1
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0  712   36
    ##          1  157 1737
    ##                                           
    ##                Accuracy : 0.9269          
    ##                  95% CI : (0.9164, 0.9366)
    ##     No Information Rate : 0.6711          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8284          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.8193          
    ##             Specificity : 0.9797          
    ##          Pos Pred Value : 0.9519          
    ##          Neg Pred Value : 0.9171          
    ##              Prevalence : 0.3289          
    ##          Detection Rate : 0.2695          
    ##    Detection Prevalence : 0.2831          
    ##       Balanced Accuracy : 0.8995          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
library(pROC)
```

    ## Warning: package 'pROC' was built under R version 4.2.2

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
probs1 <- predict(model1, test1, type = 'prob')[,2]
roc_curve <- roc(test1$move_ind, probs1)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(roc_curve)
```

![](exercise3_part2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
auc <- auc(roc_curve)
auc
```

    ## Area under the curve: 0.9749
