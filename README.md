
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

Note that name of the variables (columns) has some special characters
which is not usually the good practice and case in real studies, but for
this example we will use it as provided originally in data source.

Usually this is the first step in model development is the univariate
analysis. Let’s first perform univariate analysis using function
`univariate` from the `PDtoolkit` package:

``` r
univariate(db = loans)
```

    ##                                   rf   rf.type       bin.type            bin  cnt pct cnt.unique min     p1
    ## 1                      Creditability   numeric complete cases complete cases 1000   1          2   0   0.00
    ## 2                    Account Balance character complete cases complete cases 1000   1          4  NA     NA
    ## 3         Duration of Credit (month)   numeric complete cases complete cases 1000   1         33   4   6.00
    ## 4  Payment Status of Previous Credit character complete cases complete cases 1000   1          5  NA     NA
    ## 5                            Purpose character complete cases complete cases 1000   1         10  NA     NA
    ## 6                      Credit Amount   numeric complete cases complete cases 1000   1        923 250 425.83
    ## 7               Value Savings/Stocks character complete cases complete cases 1000   1          5  NA     NA
    ## 8       Length of current employment character complete cases complete cases 1000   1          5  NA     NA
    ## 9                Instalment per cent character complete cases complete cases 1000   1          4  NA     NA
    ## 10              Sex & Marital Status character complete cases complete cases 1000   1          4  NA     NA
    ## 11                        Guarantors character complete cases complete cases 1000   1          3  NA     NA
    ## 12       Duration in Current address character complete cases complete cases 1000   1          4  NA     NA
    ## 13     Most valuable available asset character complete cases complete cases 1000   1          4  NA     NA
    ## 14                       Age (years)   numeric complete cases complete cases 1000   1         53  19  20.00
    ## 15                Concurrent Credits character complete cases complete cases 1000   1          3  NA     NA
    ## 16                 Type of apartment character complete cases complete cases 1000   1          3  NA     NA
    ## 17        No of Credits at this Bank character complete cases complete cases 1000   1          4  NA     NA
    ## 18                        Occupation character complete cases complete cases 1000   1          4  NA     NA
    ## 19                  No of dependents character complete cases complete cases 1000   1          2  NA     NA
    ## 20                         Telephone character complete cases complete cases 1000   1          2  NA     NA
    ## 21                    Foreign Worker character complete cases complete cases 1000   1          2  NA     NA
    ##        p5    p25    p50      avg      avg.se     p75    p95      p99   max neg  pos cnt.outliers sc.ind
    ## 1    0.00    0.0    0.0    0.300  0.01449863    1.00    1.0     1.00     1   0  300            0      0
    ## 2      NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 3    6.00   12.0   18.0   20.903  0.38133320   24.00   48.0    60.00    72   0 1000           70      0
    ## 4      NA     NA     NA       NA          NA      NA     NA       NA    NA   0  960           NA      0
    ## 5      NA     NA     NA       NA          NA      NA     NA       NA    NA   0  766           NA      0
    ## 6  708.95 1365.5 2319.5 3271.248 89.26324831 3972.25 9162.7 14180.39 18424   0 1000           72      0
    ## 7      NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 8      NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 9      NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 10     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 11     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 12     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 13     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 14  22.00   27.0   33.0   35.542  0.35900295   42.00   60.0    67.01    75   0 1000           23      0
    ## 15     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 16     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 17     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 18     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 19     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 20     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 21     NA     NA     NA       NA          NA      NA     NA       NA    NA   0 1000           NA      0

For results interpretation and additional arguments of `univariate`
function, check help page `?univariate`.

From the structure and univariate results, we see that there are 4
numeric variables, while the rest are categorical. One of the numeric
variables `Creditability` presents binary indicator (0/1) of default
status which will serve as our dependent variables for PD model
development example. The rest of numeric variables are:
`Duration of Credit (month)`, `Credit Amount` and `Age (years)` and
along with other categorical variables present potential risk factors
for the PD model. Usually, when building PD models numeric risk factors
are discretized, so we will proceed next with that step. For that
purpose we will use one of the binning procedures from the `monobin`
package which serves exactly this purpose. Details about this package
can be found
[here](https://cran.r-project.org/web//packages/monobinShiny/monobinShiny.pdf).

``` r
#identify numeric risk factors
num.rf <- sapply(loans, is.numeric)
num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#discretized numeric risk factors using ndr.bin from monobin package
loans[, num.rf] <- sapply(num.rf, function(x) 
ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
#check loans structure again and confirm binning was sucessful
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
