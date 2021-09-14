
## PDtoolkit

PDtoolkit provides collection of tools for PD rating model development
and validation.</br> Having in mind the fact that model development
process is highly iterative and repetitive, of the outmost importance
for the modelers is to have standardized and automated tools for this
purpose. The main goal of this package is to cover the most common steps
of PD model development. As additional contribution author attempted to
add some functionalities which at the moment of package developemt were
not presented in `R` package ecosystem for area of credit risk models.
Procedures available are those that refer to univariate, bivariate,
multivariate analysis and calibration. </br> Along with accompanied
`monobin` and `monobinShiny` packages, `PDtoolkit` provides functions
which are suitable for different data transformation and modeling tasks
such as: imputations, monotonic binning of numeric risk factors, binning
of categorical risk factors, weights of evidence (WoE) and information
value (IV) calculations, WoE coding (replacement of risk factors
modalities with WoE values), risk factor clustering, area under curve
(AUC) calculation and others.</br> Beside mentioned features, set of
validation functions are available (homogeneity, heterogeneity as well
as discriminatory and predictive model ability testing on application
portfolio).

Following case study shows usage of `PDtoolkit` package. It is based on
publicaly available German credit data set which is available and
downloaded from this
[link](https://online.stat.psu.edu/stat857/node/215/), but also
distributed along with `PDtoolkit` package under the data frame `loans`.

First, we will import the libraries needed for this examples.

``` r
library(PDtoolkit)
library(rpart)
```

Then, let’s import and inspect the structure of the modeling data set -
`loans`.

``` r
data(loans)
str(loans)
```

    ## 'data.frame':    1000 obs. of  21 variables:
    ##  $ Creditability                    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Account Balance                  : chr  "1" "1" "2" "1" ...
    ##  $ Duration of Credit (month)       : num  18 9 12 12 12 10 8 6 18 24 ...
    ##  $ Payment Status of Previous Credit: chr  "4" "4" "2" "4" ...
    ##  $ Purpose                          : chr  "2" "0" "9" "0" ...
    ##  $ Credit Amount                    : num  1049 2799 841 2122 2171 ...
    ##  $ Value Savings/Stocks             : chr  "1" "1" "2" "1" ...
    ##  $ Length of current employment     : chr  "2" "3" "4" "3" ...
    ##  $ Instalment per cent              : chr  "4" "2" "2" "3" ...
    ##  $ Sex & Marital Status             : chr  "2" "3" "2" "3" ...
    ##  $ Guarantors                       : chr  "1" "1" "1" "1" ...
    ##  $ Duration in Current address      : chr  "4" "2" "4" "2" ...
    ##  $ Most valuable available asset    : chr  "2" "1" "1" "1" ...
    ##  $ Age (years)                      : num  21 36 23 39 38 48 39 40 65 23 ...
    ##  $ Concurrent Credits               : chr  "3" "3" "3" "3" ...
    ##  $ Type of apartment                : chr  "1" "1" "1" "1" ...
    ##  $ No of Credits at this Bank       : chr  "1" "2" "1" "2" ...
    ##  $ Occupation                       : chr  "3" "3" "2" "2" ...
    ##  $ No of dependents                 : chr  "1" "2" "1" "2" ...
    ##  $ Telephone                        : chr  "1" "1" "1" "1" ...
    ##  $ Foreign Worker                   : chr  "1" "1" "1" "2" ...

Test:

``` r
#identify numeric risk factors
num.rf <- sapply(loans, is.numeric)
num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#discretized numeric risk factors using ndr.bin from monobin package
loans[, num.rf] <- sapply(num.rf, function(x) 
ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
str(loans)
```

    ## 'data.frame':    1000 obs. of  21 variables:
    ##  $ Creditability                    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Account Balance                  : chr  "1" "1" "2" "1" ...
    ##  $ Duration of Credit (month)       : chr  "03 [16,45)" "02 [8,16)" "02 [8,16)" "02 [8,16)" ...
    ##  $ Payment Status of Previous Credit: chr  "4" "4" "2" "4" ...
    ##  $ Purpose                          : chr  "2" "0" "9" "0" ...
    ##  $ Credit Amount                    : chr  "01 [250,3914)" "01 [250,3914)" "01 [250,3914)" "01 [250,3914)" ...
    ##  $ Value Savings/Stocks             : chr  "1" "1" "2" "1" ...
    ##  $ Length of current employment     : chr  "2" "3" "4" "3" ...
    ##  $ Instalment per cent              : chr  "4" "2" "2" "3" ...
    ##  $ Sex & Marital Status             : chr  "2" "3" "2" "3" ...
    ##  $ Guarantors                       : chr  "1" "1" "1" "1" ...
    ##  $ Duration in Current address      : chr  "4" "2" "4" "2" ...
    ##  $ Most valuable available asset    : chr  "2" "1" "1" "1" ...
    ##  $ Age (years)                      : chr  "01 [19,26)" "03 [35,Inf)" "01 [19,26)" "03 [35,Inf)" ...
    ##  $ Concurrent Credits               : chr  "3" "3" "3" "3" ...
    ##  $ Type of apartment                : chr  "1" "1" "1" "1" ...
    ##  $ No of Credits at this Bank       : chr  "1" "2" "1" "2" ...
    ##  $ Occupation                       : chr  "3" "3" "2" "2" ...
    ##  $ No of dependents                 : chr  "1" "2" "1" "2" ...
    ##  $ Telephone                        : chr  "1" "1" "1" "1" ...
    ##  $ Foreign Worker                   : chr  "1" "1" "1" "2" ...

``` r
#run stepMIV
res <- stepMIV(start.model = Creditability ~ 1, 
       miv.threshold = 0.02, 
       m.ch.p.val = 0.05,
       coding = "WoE",
       db = loans)
```

    ## [1] "Running iteration: 1"
    ## [1] "Running iteration: 2"
    ## [1] "Running iteration: 3"
    ## [1] "Running iteration: 4"
    ## [1] "Running iteration: 5"
    ## [1] "Running iteration: 6"
    ## [1] "Running iteration: 7"
    ## [1] "Running iteration: 8"
    ## [1] "Running iteration: 9"

``` r
#check output elements
names(res)
```

    ## [1] "model"    "steps"    "miv.iter" "warnings" "dev.db"

``` r
#extract the final model
final.model <- res$model
#print coefficients
summary(final.model)$coefficients
```

    ##                                       Estimate Std. Error    z value     Pr(>|z|)
    ## (Intercept)                         -0.8514882 0.08231964 -10.343682 4.470231e-25
    ## `Account Balance`                   -0.8254011 0.10337123  -7.984824 1.407229e-15
    ## `Duration of Credit (month)`        -0.9314410 0.16625192  -5.602588 2.111752e-08
    ## `Payment Status of Previous Credit` -0.7496373 0.15243106  -4.917878 8.748753e-07
    ## Purpose                             -1.0103552 0.20289369  -4.979727 6.367398e-07
    ## `Value Savings/Stocks`              -0.7550344 0.19396980  -3.892536 9.920180e-05
    ## `Age (years)`                       -0.7014161 0.24873716  -2.819909 4.803728e-03
    ## `Foreign Worker`                    -1.0398828 0.46998781  -2.212574 2.692703e-02
    ## Guarantors                          -1.1169885 0.43579548  -2.563103 1.037414e-02

``` r
#print steps of stepwise
res$steps
```

    ##          target                            rf.miv        miv m.chiq.stat        p.val
    ## 1 Creditability                   Account Balance 0.66601150  131.335922 2.787203e-28
    ## 2 Creditability        Duration of Credit (month) 0.23256790   39.904108 1.116562e-08
    ## 3 Creditability Payment Status of Previous Credit 0.18497903   23.606619 9.576571e-05
    ## 4 Creditability                           Purpose 0.13598559   27.962661 9.676664e-04
    ## 5 Creditability              Value Savings/Stocks 0.11535298   14.099760 6.983375e-03
    ## 6 Creditability                       Age (years) 0.05489906    6.690154 3.525750e-02
    ## 7 Creditability                    Foreign Worker 0.03587842    5.118958 2.366582e-02
    ## 8 Creditability                        Guarantors 0.02833412    6.112445 4.706516e-02

``` r
#print head of all iteration details
head(res$miv.iter)
```

    ##   iter                                rf bin no.o ng.o nb.o      woe.o no.e  ng.e  nb.e
    ## 1    1                   Account Balance   1  274  139  135 -0.8180987  274 191.8  82.2
    ## 2    1                   Account Balance   2  269  164  105 -0.4013918  269 188.3  80.7
    ## 3    1                   Account Balance   3   63   49   14  0.4054651   63  44.1  18.9
    ## 4    1                   Account Balance   4  394  348   46  1.1762632  394 275.8 118.2
    ## 5    1 Payment Status of Previous Credit   0   40   15   25 -1.3581235   40  28.0  12.0
    ## 6    1 Payment Status of Previous Credit   1   49   21   28 -1.1349799   49  34.3  14.7
    ##                      woe.e      delta miv.val.g  miv.val.b       miv m.chiq.gb m.chiq.stat
    ## 1 0.0000000000000000000000 -0.8180987 0.3566620 -0.3093495 0.6660115 22.221016   131.33592
    ## 2 0.0000000000000000000000 -0.4013918 0.3566620 -0.3093495 0.6660115  4.978405   131.33592
    ## 3 0.0000000000000000000000  0.4054651 0.3566620 -0.3093495 0.6660115  0.961201   131.33592
    ## 4 0.0000000000000002220446  1.1762632 0.3566620 -0.3093495 0.6660115 37.507338   131.33592
    ## 5 0.0000000000000002220446 -1.3581235 0.1387143 -0.1545192 0.2932335  8.986915    60.46711
    ## 6 0.0000000000000002220446 -1.1349799 0.1387143 -0.1545192 0.2932335  7.738915    60.46711
    ##          p.val
    ## 1 2.787203e-28
    ## 2 2.787203e-28
    ## 3 2.787203e-28
    ## 4 2.787203e-28
    ## 5 2.313958e-12
    ## 6 2.313958e-12

``` r
#print warnings
res$warnings
```

    ##                                  rf                                comment
    ## 1 Payment Status of Previous Credit At least one pct per bin less then 5%.
    ## 2                           Purpose At least one pct per bin less then 5%.
    ## 3              Value Savings/Stocks At least one pct per bin less then 5%.
    ## 4                        Guarantors At least one pct per bin less then 5%.
    ## 5                Concurrent Credits At least one pct per bin less then 5%.
    ## 6        No of Credits at this Bank At least one pct per bin less then 5%.
    ## 7                        Occupation At least one pct per bin less then 5%.
    ## 8                    Foreign Worker At least one pct per bin less then 5%.

``` r
#print head of coded development data
head(res$dev.db)
```

    ##   Creditability Account Balance Duration of Credit (month) Payment Status of Previous Credit
    ## 1             0      -0.8180987                 -0.1959478                        0.73374058
    ## 2             0      -0.8180987                  0.3466246                        0.73374058
    ## 3             0      -0.4013918                  0.3466246                       -0.08831862
    ## 4             0      -0.8180987                  0.3466246                        0.73374058
    ## 5             0      -0.8180987                  0.3466246                        0.73374058
    ## 6             0      -0.8180987                  0.3466246                        0.73374058
    ##       Purpose Credit Amount Value Savings/Stocks Length of current employment Instalment per cent
    ## 1 -0.09555652 01 [250,3914)           -0.2713578                            2                   4
    ## 2 -0.35920049 01 [250,3914)           -0.2713578                            3                   2
    ## 3 -0.23052366 01 [250,3914)           -0.1395519                            4                   2
    ## 4 -0.35920049 01 [250,3914)           -0.2713578                            3                   3
    ## 5 -0.35920049 01 [250,3914)           -0.2713578                            3                   4
    ## 6 -0.35920049 01 [250,3914)           -0.2713578                            2                   1
    ##   Sex & Marital Status   Guarantors Duration in Current address Most valuable available asset
    ## 1                    2 0.0005250722                           4                             2
    ## 2                    3 0.0005250722                           2                             1
    ## 3                    2 0.0005250722                           4                             1
    ## 4                    3 0.0005250722                           2                             1
    ## 5                    3 0.0005250722                           4                             2
    ## 6                    3 0.0005250722                           3                             1
    ##   Age (years) Concurrent Credits Type of apartment No of Credits at this Bank Occupation
    ## 1  -0.5288441                  3                 1                          1          3
    ## 2   0.3141153                  3                 1                          2          3
    ## 3  -0.5288441                  3                 1                          1          2
    ## 4   0.3141153                  3                 1                          2          2
    ## 5   0.3141153                  1                 2                          2          2
    ## 6   0.3141153                  3                 1                          2          2
    ##   No of dependents Telephone Foreign Worker
    ## 1                1         1    -0.03486727
    ## 2                2         1    -0.03486727
    ## 3                1         1    -0.03486727
    ## 4                2         1     1.26291534
    ## 5                1         1     1.26291534
    ## 6                2         1     1.26291534

``` r
#calculate AUC
auc.model(predictions = predict(final.model, type = "response", newdata = res$dev.db),
    observed = res$dev.db$Creditability)
```

    ## [1] 0.8096214
