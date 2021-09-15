
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
Additionally, in order to present all functionalities of PDtoolkit
package, we will slightly modify modeling data set inserting some
missing values for certain variables as given below:

``` r
loans$"Credit Amount"[1:10] <- NA
loans$"Age (years)"[30:50] <- NA
loans$"Purpose"[800:820] <- NA
```

Usually this is the first step in model development is the univariate
analysis. For this purpose we will use `univariate` function from the
`PDtoolkit` package:

``` r
univariate(db = loans)
```

    ##                                   rf   rf.type       bin.type            bin  cnt   pct cnt.unique min     p1     p5     p25
    ## 1                      Creditability   numeric complete cases complete cases 1000 1.000          2   0   0.00   0.00    0.00
    ## 2                    Account Balance character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 3         Duration of Credit (month)   numeric complete cases complete cases 1000 1.000         33   4   6.00   6.00   12.00
    ## 4  Payment Status of Previous Credit character complete cases complete cases 1000 1.000          5  NA     NA     NA      NA
    ## 5                            Purpose character  special cases  special cases   21 0.021          1  NA     NA     NA      NA
    ## 6                            Purpose character complete cases complete cases  979 0.979         10  NA     NA     NA      NA
    ## 7                      Credit Amount   numeric  special cases  special cases   10 0.010          1 Inf     NA     NA      NA
    ## 8                      Credit Amount   numeric complete cases complete cases  990 0.990        915 250 424.13 708.45 1371.25
    ## 9               Value Savings/Stocks character complete cases complete cases 1000 1.000          5  NA     NA     NA      NA
    ## 10      Length of current employment character complete cases complete cases 1000 1.000          5  NA     NA     NA      NA
    ## 11               Instalment per cent character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 12              Sex & Marital Status character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 13                        Guarantors character complete cases complete cases 1000 1.000          3  NA     NA     NA      NA
    ## 14       Duration in Current address character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 15     Most valuable available asset character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 16                       Age (years)   numeric  special cases  special cases   21 0.021          1 Inf     NA     NA      NA
    ## 17                       Age (years)   numeric complete cases complete cases  979 0.979         53  19  20.00  22.00   27.00
    ## 18                Concurrent Credits character complete cases complete cases 1000 1.000          3  NA     NA     NA      NA
    ## 19                 Type of apartment character complete cases complete cases 1000 1.000          3  NA     NA     NA      NA
    ## 20        No of Credits at this Bank character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 21                        Occupation character complete cases complete cases 1000 1.000          4  NA     NA     NA      NA
    ## 22                  No of dependents character complete cases complete cases 1000 1.000          2  NA     NA     NA      NA
    ## 23                         Telephone character complete cases complete cases 1000 1.000          2  NA     NA     NA      NA
    ## 24                    Foreign Worker character complete cases complete cases 1000 1.000          2  NA     NA     NA      NA
    ##     p50        avg      avg.se     p75    p95      p99   max neg  pos cnt.outliers sc.ind
    ## 1     0    0.30000  0.01449863    1.00    1.0     1.00     1   0  300            0      0
    ## 2    NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 3    18   20.90300  0.38133320   24.00   48.0    60.00    72   0 1000           70      0
    ## 4    NA         NA          NA      NA     NA       NA    NA   0  960           NA      0
    ## 5    NA         NA          NA      NA     NA       NA    NA  NA   NA           NA      0
    ## 6    NA         NA          NA      NA     NA       NA    NA   0  755           NA      0
    ## 7    NA        NaN          NA      NA     NA       NA  -Inf  NA   NA            0      0
    ## 8  2324 3283.24242 90.03256824 3978.25 9219.7 14194.29 18424   0  990           72      0
    ## 9    NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 10   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 11   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 12   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 13   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 14   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 15   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 16   NA        NaN          NA      NA     NA       NA  -Inf  NA   NA            0      0
    ## 17   33   35.52605  0.36315861   42.00   60.0    67.22    75   0  979           23      0
    ## 18   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 19   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 20   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 21   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 22   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 23   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0
    ## 24   NA         NA          NA      NA     NA       NA    NA   0 1000           NA      0

Based on the results we can see that `univariate` treats differently so
called special and complete cases. For result details and additional
arguments of `univariate` function check help page `?univariate`.

From the structure and univariate results, we see that there are 4
numeric variables, while the rest are categorical. One of the numeric
variables `Creditability` presents binary indicator (0/1) of default
status which will serve as our dependent variables for PD model
development. The rest of the variables present potential risk factors
for the PD model. Additionally, we see that some risk factors
(`Purpose`, `Credit Amount`, `Age (years)`) have certain share of
special cases and that for numeric risk factors potential outliers are
identified. Sometimes, modelers want to impute certian values instead of
special cases (usually missing values) as well as for potential
outliers. Without going to much into deeper analysis for this case study
we can perform imputations using two functions from `PDtoolkit`package:
`imp.sc` for special cases and `imp.outliers` for outliers.

``` r
#imputation for special cases for risk factors "Credit Amount", "Purpose"
imp.sc.res <- imp.sc(db = loans[, c("Credit Amount", "Purpose")], 
               method.num = "automatic",
               p.val = 0.05)
names(imp.sc.res)
```

    ## [1] "db"     "report"

``` r
#new risk factors with imputed values
head(imp.sc.res[[1]])
```

    ##   Credit Amount Purpose
    ## 1          2324       2
    ## 2          2324       0
    ## 3          2324       9
    ## 4          2324       0
    ## 5          2324       0
    ## 6          2324       0

``` r
#imputation report
imp.sc.res[[2]]
```

    ##              rf                  info  imputation.method imputed.value imputation.num imputed.mode
    ## 1 Credit Amount Imputation completed. automatic - median          2324             10         <NA>
    ## 2       Purpose Imputation completed.               mode            NA             21            3

``` r
#replace Credit Amount and Purpose with new values
loans[, c("Credit Amount", "Purpose")] <- imp.sc.res[[1]]
colSums(is.na(loans))
```

    ##                     Creditability                   Account Balance        Duration of Credit (month) 
    ##                                 0                                 0                                 0 
    ## Payment Status of Previous Credit                           Purpose                     Credit Amount 
    ##                                 0                                 0                                 0 
    ##              Value Savings/Stocks      Length of current employment               Instalment per cent 
    ##                                 0                                 0                                 0 
    ##              Sex & Marital Status                        Guarantors       Duration in Current address 
    ##                                 0                                 0                                 0 
    ##     Most valuable available asset                       Age (years)                Concurrent Credits 
    ##                                 0                                21                                 0 
    ##                 Type of apartment        No of Credits at this Bank                        Occupation 
    ##                                 0                                 0                                 0 
    ##                  No of dependents                         Telephone                    Foreign Worker 
    ##                                 0                                 0                                 0

``` r
#imputation for outliers for risk factor 
imp.out.res <- imp.outliers(db = loans[, "Credit Amount", drop = FALSE], 
                    method = "iqr",
                    range = 1.5)
#new risk factors with imputed values
head(imp.out.res[[1]])
```

    ##   Credit Amount
    ## 1          2324
    ## 2          2324
    ## 3          2324
    ## 4          2324
    ## 5          2324
    ## 6          2324

``` r
#imputation report
imp.out.res[[2]]
```

    ##              rf                  info imputation.method imputation.val.upper imputation.val.lower imputation.num.upper
    ## 1 Credit Amount Imputation completed.               iqr                 9214                708.5                   50
    ##   imputation.num.lower
    ## 1                   50

``` r
#replace Credit Amount with new values
loans[, "Credit Amount"] <- imp.out.res[[1]]
```

Usually, when building PD models numeric risk factors are discretized,
so we will proceed next with that step. For purpose of binning the
numeric risk factors, we will use one of the functions from the
`monobin` package. Details about this package can be found
[here](https://cran.r-project.org/web//packages/monobinShiny/monobinShiny.pdf).

``` r
#identify numeric risk factors
num.rf <- sapply(loans, is.numeric)
num.rf <- names(num.rf)[!names(num.rf)%in%"Creditability" & num.rf]
#discretized numeric risk factors using ndr.bin from monobin package
loans[, num.rf] <- sapply(num.rf, function(x) 
                  monobin::ndr.bin(x = loans[, x], y = loans[, "Creditability"])[[2]])
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

As we can see after binning, all risk factors are now categorical. Now
the modeling data set is ready for bivariate analysis. For that purpose
we will use the same name function `bivariate` from the
`PDtoolkit`package.

``` r
bivariate(db = loans, target = "Creditability")
```

    ## $results
    ##                                   rf           bin  no  ng  nb pct.o       pct.g       pct.b        dr   so  sg  sb
    ## 1                    Account Balance             1 274 139 135 0.274 0.198571429 0.450000000 0.4927007 1000 700 300
    ## 2                    Account Balance             2 269 164 105 0.269 0.234285714 0.350000000 0.3903346 1000 700 300
    ## 3                    Account Balance             3  63  49  14 0.063 0.070000000 0.046666667 0.2222222 1000 700 300
    ## 4                    Account Balance             4 394 348  46 0.394 0.497142857 0.153333333 0.1167513 1000 700 300
    ## 5         Duration of Credit (month)      01 [4,8)  87  78   9 0.087 0.111428571 0.030000000 0.1034483 1000 700 300
    ## 6         Duration of Credit (month)     02 [8,16) 344 264  80 0.344 0.377142857 0.266666667 0.2325581 1000 700 300
    ## 7         Duration of Credit (month)    03 [16,45) 499 328 171 0.499 0.468571429 0.570000000 0.3426854 1000 700 300
    ## 8         Duration of Credit (month)   04 [45,Inf)  70  30  40 0.070 0.042857143 0.133333333 0.5714286 1000 700 300
    ## 9  Payment Status of Previous Credit             0  40  15  25 0.040 0.021428571 0.083333333 0.6250000 1000 700 300
    ## 10 Payment Status of Previous Credit             1  49  21  28 0.049 0.030000000 0.093333333 0.5714286 1000 700 300
    ## 11 Payment Status of Previous Credit             2 530 361 169 0.530 0.515714286 0.563333333 0.3188679 1000 700 300
    ## 12 Payment Status of Previous Credit             3  88  60  28 0.088 0.085714286 0.093333333 0.3181818 1000 700 300
    ## 13 Payment Status of Previous Credit             4 293 243  50 0.293 0.347142857 0.166666667 0.1706485 1000 700 300
    ## 14                           Purpose             0 224 145  79 0.224 0.207142857 0.263333333 0.3526786 1000 700 300
    ## 15                           Purpose             1 102  86  16 0.102 0.122857143 0.053333333 0.1568627 1000 700 300
    ## 16                           Purpose            10  12   7   5 0.012 0.010000000 0.016666667 0.4166667 1000 700 300
    ## 17                           Purpose             2 177 123  54 0.177 0.175714286 0.180000000 0.3050847 1000 700 300
    ## 18                           Purpose             3 300 218  82 0.300 0.311428571 0.273333333 0.2733333 1000 700 300
    ## 19                           Purpose             4  12   8   4 0.012 0.011428571 0.013333333 0.3333333 1000 700 300
    ## 20                           Purpose             5  21  14   7 0.021 0.020000000 0.023333333 0.3333333 1000 700 300
    ## 21                           Purpose             6  49  28  21 0.049 0.040000000 0.070000000 0.4285714 1000 700 300
    ## 22                           Purpose             8   9   8   1 0.009 0.011428571 0.003333333 0.1111111 1000 700 300
    ## 23                           Purpose             9  94  63  31 0.094 0.090000000 0.103333333 0.3297872 1000 700 300
    ## 24                     Credit Amount 01 [250,3914) 739 550 189 0.739 0.785714286 0.630000000 0.2557510 1000 700 300
    ## 25                     Credit Amount 02 [3914,Inf) 261 150 111 0.261 0.214285714 0.370000000 0.4252874 1000 700 300
    ## 26              Value Savings/Stocks             1 603 386 217 0.603 0.551428571 0.723333333 0.3598673 1000 700 300
    ## 27              Value Savings/Stocks             2 103  69  34 0.103 0.098571429 0.113333333 0.3300971 1000 700 300
    ## 28              Value Savings/Stocks             3  63  52  11 0.063 0.074285714 0.036666667 0.1746032 1000 700 300
    ## 29              Value Savings/Stocks             4  48  42   6 0.048 0.060000000 0.020000000 0.1250000 1000 700 300
    ## 30              Value Savings/Stocks             5 183 151  32 0.183 0.215714286 0.106666667 0.1748634 1000 700 300
    ## 31      Length of current employment             1  62  39  23 0.062 0.055714286 0.076666667 0.3709677 1000 700 300
    ## 32      Length of current employment             2 172 102  70 0.172 0.145714286 0.233333333 0.4069767 1000 700 300
    ## 33      Length of current employment             3 339 235 104 0.339 0.335714286 0.346666667 0.3067847 1000 700 300
    ## 34      Length of current employment             4 174 135  39 0.174 0.192857143 0.130000000 0.2241379 1000 700 300
    ## 35      Length of current employment             5 253 189  64 0.253 0.270000000 0.213333333 0.2529644 1000 700 300
    ## 36               Instalment per cent             1 136 102  34 0.136 0.145714286 0.113333333 0.2500000 1000 700 300
    ## 37               Instalment per cent             2 231 169  62 0.231 0.241428571 0.206666667 0.2683983 1000 700 300
    ## 38               Instalment per cent             3 157 112  45 0.157 0.160000000 0.150000000 0.2866242 1000 700 300
    ## 39               Instalment per cent             4 476 317 159 0.476 0.452857143 0.530000000 0.3340336 1000 700 300
    ## 40              Sex & Marital Status             1  50  30  20 0.050 0.042857143 0.066666667 0.4000000 1000 700 300
    ## 41              Sex & Marital Status             2 310 201 109 0.310 0.287142857 0.363333333 0.3516129 1000 700 300
    ## 42              Sex & Marital Status             3 548 402 146 0.548 0.574285714 0.486666667 0.2664234 1000 700 300
    ## 43              Sex & Marital Status             4  92  67  25 0.092 0.095714286 0.083333333 0.2717391 1000 700 300
    ## 44                        Guarantors             1 907 635 272 0.907 0.907142857 0.906666667 0.2998897 1000 700 300
    ## 45                        Guarantors             2  41  23  18 0.041 0.032857143 0.060000000 0.4390244 1000 700 300
    ## 46                        Guarantors             3  52  42  10 0.052 0.060000000 0.033333333 0.1923077 1000 700 300
    ## 47       Duration in Current address             1 130  94  36 0.130 0.134285714 0.120000000 0.2769231 1000 700 300
    ## 48       Duration in Current address             2 308 211  97 0.308 0.301428571 0.323333333 0.3149351 1000 700 300
    ## 49       Duration in Current address             3 149 106  43 0.149 0.151428571 0.143333333 0.2885906 1000 700 300
    ## 50       Duration in Current address             4 413 289 124 0.413 0.412857143 0.413333333 0.3002421 1000 700 300
    ## 51     Most valuable available asset             1 282 222  60 0.282 0.317142857 0.200000000 0.2127660 1000 700 300
    ## 52     Most valuable available asset             2 232 161  71 0.232 0.230000000 0.236666667 0.3060345 1000 700 300
    ## 53     Most valuable available asset             3 332 230 102 0.332 0.328571429 0.340000000 0.3072289 1000 700 300
    ## 54     Most valuable available asset             4 154  87  67 0.154 0.124285714 0.223333333 0.4350649 1000 700 300
    ## 55                       Age (years)    01 [19,26) 187 108  79 0.187 0.154285714 0.263333333 0.4224599 1000 700 300
    ## 56                       Age (years)    02 [26,35) 349 238 111 0.349 0.340000000 0.370000000 0.3180516 1000 700 300
    ## 57                       Age (years)   03 [35,Inf) 443 335 108 0.443 0.478571429 0.360000000 0.2437923 1000 700 300
    ## 58                       Age (years)            SC  21  19   2 0.021 0.027142857 0.006666667 0.0952381 1000 700 300
    ## 59                Concurrent Credits             1 139  82  57 0.139 0.117142857 0.190000000 0.4100719 1000 700 300
    ## 60                Concurrent Credits             2  47  28  19 0.047 0.040000000 0.063333333 0.4042553 1000 700 300
    ## 61                Concurrent Credits             3 814 590 224 0.814 0.842857143 0.746666667 0.2751843 1000 700 300
    ## 62                 Type of apartment             1 179 109  70 0.179 0.155714286 0.233333333 0.3910615 1000 700 300
    ## 63                 Type of apartment             2 714 528 186 0.714 0.754285714 0.620000000 0.2605042 1000 700 300
    ## 64                 Type of apartment             3 107  63  44 0.107 0.090000000 0.146666667 0.4112150 1000 700 300
    ## 65        No of Credits at this Bank             1 633 433 200 0.633 0.618571429 0.666666667 0.3159558 1000 700 300
    ## 66        No of Credits at this Bank             2 333 241  92 0.333 0.344285714 0.306666667 0.2762763 1000 700 300
    ## 67        No of Credits at this Bank             3  28  22   6 0.028 0.031428571 0.020000000 0.2142857 1000 700 300
    ## 68        No of Credits at this Bank             4   6   4   2 0.006 0.005714286 0.006666667 0.3333333 1000 700 300
    ## 69                        Occupation             1  22  15   7 0.022 0.021428571 0.023333333 0.3181818 1000 700 300
    ## 70                        Occupation             2 200 144  56 0.200 0.205714286 0.186666667 0.2800000 1000 700 300
    ## 71                        Occupation             3 630 444 186 0.630 0.634285714 0.620000000 0.2952381 1000 700 300
    ## 72                        Occupation             4 148  97  51 0.148 0.138571429 0.170000000 0.3445946 1000 700 300
    ## 73                  No of dependents             1 845 591 254 0.845 0.844285714 0.846666667 0.3005917 1000 700 300
    ## 74                  No of dependents             2 155 109  46 0.155 0.155714286 0.153333333 0.2967742 1000 700 300
    ## 75                         Telephone             1 596 409 187 0.596 0.584285714 0.623333333 0.3137584 1000 700 300
    ## 76                         Telephone             2 404 291 113 0.404 0.415714286 0.376666667 0.2797030 1000 700 300
    ## 77                    Foreign Worker             1 963 667 296 0.963 0.952857143 0.986666667 0.3073728 1000 700 300
    ## 78                    Foreign Worker             2  37  33   4 0.037 0.047142857 0.013333333 0.1081081 1000 700 300
    ##         dist.g      dist.b           woe            iv.b          iv.s       auc
    ## 1  0.198571429 0.450000000 -0.8180987057 0.2056933888604 0.66601150335 0.7077690
    ## 2  0.234285714 0.350000000 -0.4013917827 0.0464467634291 0.66601150335 0.7077690
    ## 3  0.070000000 0.046666667  0.4054651081 0.0094608525225 0.66601150335 0.7077690
    ## 4  0.497142857 0.153333333  1.1762632229 0.4044104985393 0.66601150335 0.7077690
    ## 5  0.111428571 0.030000000  1.3121863890 0.1068494631015 0.26770659621 0.6241762
    ## 6  0.377142857 0.266666667  0.3466246081 0.0382937662266 0.26770659621 0.6241762
    ## 7  0.468571429 0.570000000 -0.1959478085 0.0198747062913 0.26770659621 0.6241762
    ## 8  0.042857143 0.133333333 -1.1349799328 0.1026886605902 0.26770659621 0.6241762
    ## 9  0.021428571 0.083333333 -1.3581234842 0.0840743109238 0.29323354739 0.6268048
    ## 10 0.030000000 0.093333333 -1.1349799328 0.0718820624131 0.29323354739 0.6268048
    ## 11 0.515714286 0.563333333 -0.0883186170 0.0042056484275 0.29323354739 0.6268048
    ## 12 0.085714286 0.093333333 -0.0851578083 0.0006488213969 0.29323354739 0.6268048
    ## 13 0.347142857 0.166666667  0.7337405775 0.1324227042295 0.29323354739 0.6268048
    ## 14 0.207142857 0.263333333 -0.2400119704 0.0134863869101 0.10939307333 0.5821500
    ## 15 0.122857143 0.053333333  0.8344607136 0.0580148877093 0.10939307333 0.5821500
    ## 16 0.010000000 0.016666667 -0.5108256238 0.0034055041584 0.10939307333 0.5821500
    ## 17 0.175714286 0.180000000 -0.0240975516 0.0001032752211 0.10939307333 0.5821500
    ## 18 0.311428571 0.273333333  0.1304779551 0.0049705887671 0.10939307333 0.5821500
    ## 19 0.011428571 0.013333333 -0.1541506798 0.0002936203425 0.10939307333 0.5821500
    ## 20 0.020000000 0.023333333 -0.1541506798 0.0005138355994 0.10939307333 0.5821500
    ## 21 0.040000000 0.070000000 -0.5596157879 0.0167884736381 0.10939307333 0.5821500
    ## 22 0.011428571 0.003333333  1.2321436813 0.0099744964676 0.10939307333 0.5821500
    ## 23 0.090000000 0.103333333 -0.1381503385 0.0018420045131 0.10939307333 0.5821500
    ## 24 0.785714286 0.630000000  0.2208734028 0.0343931441471 0.11944316082 0.5778571
    ## 25 0.214285714 0.370000000 -0.5461927676 0.0850500166697 0.11944316082 0.5778571
    ## 26 0.551428571 0.723333333 -0.2713578445 0.0466477056434 0.19600955690 0.5991429
    ## 27 0.098571429 0.113333333 -0.1395518804 0.0020600515679 0.19600955690 0.5991429
    ## 28 0.074285714 0.036666667  0.7060505854 0.0265609505935 0.19600955690 0.5991429
    ## 29 0.060000000 0.020000000  1.0986122887 0.0439444915467 0.19600955690 0.5991429
    ## 30 0.215714286 0.106666667  0.7042460736 0.0767963575528 0.19600955690 0.5991429
    ## 31 0.055714286 0.076666667 -0.3192304302 0.0066886375849 0.08643363103 0.5808190
    ## 32 0.145714286 0.233333333 -0.4708202892 0.0412528253352 0.08643363103 0.5808190
    ## 33 0.335714286 0.346666667 -0.0321032454 0.0003516069733 0.08643363103 0.5808190
    ## 34 0.192857143 0.130000000  0.3944152719 0.0247918170922 0.08643363103 0.5808190
    ## 35 0.270000000 0.213333333  0.2355660713 0.0133487440411 0.08643363103 0.5808190
    ## 36 0.145714286 0.113333333  0.2513144283 0.0081378005348 0.02632209005 0.5433833
    ## 37 0.241428571 0.206666667  0.1554664695 0.0054043106061 0.02632209005 0.5433833
    ## 38 0.160000000 0.150000000  0.0645385211 0.0006453852114 0.02632209005 0.5433833
    ## 39 0.452857143 0.530000000 -0.1573002887 0.0121345937020 0.02632209005 0.5433833
    ## 40 0.042857143 0.066666667 -0.4418327523 0.0105198274352 0.04467067763 0.5524238
    ## 41 0.287142857 0.363333333 -0.2353408346 0.0179307302520 0.04467067763 0.5524238
    ## 42 0.574285714 0.486666667  0.1655476065 0.0145051236192 0.04467067763 0.5524238
    ## 43 0.095714286 0.083333333  0.1385189341 0.0017149963274 0.04467067763 0.5524238
    ## 44 0.907142857 0.906666667  0.0005250722 0.0000002500344 0.03201932202 0.5256524
    ## 45 0.032857143 0.060000000 -0.6021754024 0.0163447609210 0.03201932202 0.5256524
    ## 46 0.060000000 0.033333333  0.5877866649 0.0156743110641 0.03201932202 0.5256524
    ## 47 0.134285714 0.120000000  0.1124779834 0.0016068283347 0.00358877319 0.5161786
    ## 48 0.301428571 0.323333333 -0.0701507054 0.0015366344996 0.00358877319 0.5161786
    ## 49 0.151428571 0.143333333  0.0549411180 0.0004447614317 0.00358877319 0.5161786
    ## 50 0.412857143 0.413333333 -0.0011527379 0.0000005489228 0.00358877319 0.5161786
    ## 51 0.317142857 0.200000000  0.4610349593 0.0540069523708 0.11263826241 0.5853286
    ## 52 0.230000000 0.236666667 -0.0285733724 0.0001904891496 0.11263826241 0.5853286
    ## 53 0.328571429 0.340000000 -0.0341913647 0.0003907584543 0.11263826241 0.5853286
    ## 54 0.124285714 0.223333333 -0.5860823611 0.0580500624351 0.11263826241 0.5853286
    ## 55 0.154285714 0.263333333 -0.5346144857 0.0582984367772 0.12334106269 0.5890381
    ## 56 0.340000000 0.370000000 -0.0845573880 0.0025367216408 0.12334106269 0.5890381
    ## 57 0.478571429 0.360000000  0.2847014443 0.0337574569686 0.12334106269 0.5890381
    ## 58 0.027142857 0.006666667  1.4039939382 0.0287484473064 0.12334106269 0.5890381
    ## 59 0.117142857 0.190000000 -0.4836298810 0.0352358913269 0.05761454196 0.5481857
    ## 60 0.040000000 0.063333333 -0.4595323294 0.0107224210188 0.05761454196 0.5481857
    ## 61 0.842857143 0.746666667  0.1211786247 0.0116562296099 0.05761454196 0.5481857
    ## 62 0.155714286 0.233333333 -0.4044452202 0.0313926528066 0.08539292555 0.5680619
    ## 63 0.754285714 0.620000000  0.1960517496 0.0263269492328 0.08539292555 0.5680619
    ## 64 0.090000000 0.146666667 -0.4883527679 0.0276733235151 0.08539292555 0.5680619
    ## 65 0.618571429 0.666666667 -0.0748774989 0.0036012511391 0.01326652424 0.5260571
    ## 66 0.344285714 0.306666667  0.1157104961 0.0043529186611 0.01326652424 0.5260571
    ## 67 0.031428571 0.020000000  0.4519851237 0.0051655442713 0.01326652424 0.5260571
    ## 68 0.005714286 0.006666667 -0.1541506798 0.0001468101713 0.01326652424 0.5260571
    ## 69 0.021428571 0.023333333 -0.0851578083 0.0001622053492 0.00876276571 0.5214429
    ## 70 0.205714286 0.186666667  0.0971637485 0.0018507380658 0.00876276571 0.5214429
    ## 71 0.634285714 0.620000000  0.0227800283 0.0003254289762 0.00876276571 0.5214429
    ## 72 0.138571429 0.170000000 -0.2044125146 0.0064243933163 0.00876276571 0.5214429
    ## 73 0.844285714 0.846666667 -0.0028161100 0.0000067050238 0.00004339223 0.5011905
    ## 74 0.155714286 0.153333333  0.0154086254 0.0000366872032 0.00004339223 0.5011905
    ## 75 0.584285714 0.623333333 -0.0646913212 0.0025260420659 0.00637760503 0.5195238
    ## 76 0.415714286 0.376666667  0.0986375881 0.0038515629628 0.00637760503 0.5195238
    ## 77 0.952857143 0.986666667 -0.0348672688 0.0011788457545 0.04387741201 0.5169048
    ## 78 0.047142857 0.013333333  1.2629153400 0.0426985662558 0.04387741201 0.5169048
    ## 
    ## $info
    ## data frame with 0 columns and 0 rows

Based on results of bivariate analysis we can see that risk factor
`Purpose` has 10 modalities, that `Age (years)` has share of special
cases 2.1% and that there are quite some other risk factors with
modalitity share less than 5%. In order to correct above potential
issues, we can further categorize all risk factors that do not satisfy
the following conditions: each modality has at least 5% of the
observations and 1% of defaults, the maximum number of modalities cannot
be greater than 5, if special cases exist to be merged with a closest
modality (based on default rate). For this purpose we will use function
`cat.bin`. Details about usage of
``` cat.bin`` function can be found in its help page ```?cat.bin\`\`\`.

``` r
rf <- names(loans)[!names(loans)%in%"Creditability"]
loans[, rf] <- sapply(rf, function(x) 
               cat.bin(x = loans[, x], 
                     y = loans[, "Creditability"],
                     sc = c("SC", NA),
                     sc.merge = "closest",
                     min.pct.obs = 0.05, 
                     min.avg.rate = 0.01,
                     max.groups = 5, 
                     force.trend = "dr")[[2]])
#run bivariate analysis again
bivariate(db = loans, target = "Creditability")
```

    ## $results
    ##                                   rf               bin   no  ng  nb pct.o      pct.g      pct.b        dr   so  sg  sb
    ## 1                    Account Balance             1 [4]  394 348  46 0.394 0.49714286 0.15333333 0.1167513 1000 700 300
    ## 2                    Account Balance             2 [3]   63  49  14 0.063 0.07000000 0.04666667 0.2222222 1000 700 300
    ## 3                    Account Balance             3 [2]  269 164 105 0.269 0.23428571 0.35000000 0.3903346 1000 700 300
    ## 4                    Account Balance             4 [1]  274 139 135 0.274 0.19857143 0.45000000 0.4927007 1000 700 300
    ## 5         Duration of Credit (month)      1 [01 [4,8)]   87  78   9 0.087 0.11142857 0.03000000 0.1034483 1000 700 300
    ## 6         Duration of Credit (month)     2 [02 [8,16)]  344 264  80 0.344 0.37714286 0.26666667 0.2325581 1000 700 300
    ## 7         Duration of Credit (month)    3 [03 [16,45)]  499 328 171 0.499 0.46857143 0.57000000 0.3426854 1000 700 300
    ## 8         Duration of Credit (month)   4 [04 [45,Inf)]   70  30  40 0.070 0.04285714 0.13333333 0.5714286 1000 700 300
    ## 9  Payment Status of Previous Credit             1 [4]  293 243  50 0.293 0.34714286 0.16666667 0.1706485 1000 700 300
    ## 10 Payment Status of Previous Credit             2 [3]   88  60  28 0.088 0.08571429 0.09333333 0.3181818 1000 700 300
    ## 11 Payment Status of Previous Credit             3 [2]  530 361 169 0.530 0.51571429 0.56333333 0.3188679 1000 700 300
    ## 12 Payment Status of Previous Credit           4 [1,0]   89  36  53 0.089 0.05142857 0.17666667 0.5955056 1000 700 300
    ## 13                           Purpose           1 [1,8]  111  94  17 0.111 0.13428571 0.05666667 0.1531532 1000 700 300
    ## 14                           Purpose             2 [3]  300 218  82 0.300 0.31142857 0.27333333 0.2733333 1000 700 300
    ## 15                           Purpose             3 [2]  177 123  54 0.177 0.17571429 0.18000000 0.3050847 1000 700 300
    ## 16                           Purpose       4 [0,4,5,9]  351 230 121 0.351 0.32857143 0.40333333 0.3447293 1000 700 300
    ## 17                           Purpose          5 [10,6]   61  35  26 0.061 0.05000000 0.08666667 0.4262295 1000 700 300
    ## 18                     Credit Amount 1 [01 [250,3914)]  739 550 189 0.739 0.78571429 0.63000000 0.2557510 1000 700 300
    ## 19                     Credit Amount 2 [02 [3914,Inf)]  261 150 111 0.261 0.21428571 0.37000000 0.4252874 1000 700 300
    ## 20              Value Savings/Stocks           1 [4,3]  111  94  17 0.111 0.13428571 0.05666667 0.1531532 1000 700 300
    ## 21              Value Savings/Stocks             2 [5]  183 151  32 0.183 0.21571429 0.10666667 0.1748634 1000 700 300
    ## 22              Value Savings/Stocks             3 [2]  103  69  34 0.103 0.09857143 0.11333333 0.3300971 1000 700 300
    ## 23              Value Savings/Stocks             4 [1]  603 386 217 0.603 0.55142857 0.72333333 0.3598673 1000 700 300
    ## 24      Length of current employment             1 [4]  174 135  39 0.174 0.19285714 0.13000000 0.2241379 1000 700 300
    ## 25      Length of current employment             2 [5]  253 189  64 0.253 0.27000000 0.21333333 0.2529644 1000 700 300
    ## 26      Length of current employment             3 [3]  339 235 104 0.339 0.33571429 0.34666667 0.3067847 1000 700 300
    ## 27      Length of current employment             4 [1]   62  39  23 0.062 0.05571429 0.07666667 0.3709677 1000 700 300
    ## 28      Length of current employment             5 [2]  172 102  70 0.172 0.14571429 0.23333333 0.4069767 1000 700 300
    ## 29               Instalment per cent             1 [1]  136 102  34 0.136 0.14571429 0.11333333 0.2500000 1000 700 300
    ## 30               Instalment per cent             2 [2]  231 169  62 0.231 0.24142857 0.20666667 0.2683983 1000 700 300
    ## 31               Instalment per cent             3 [3]  157 112  45 0.157 0.16000000 0.15000000 0.2866242 1000 700 300
    ## 32               Instalment per cent             4 [4]  476 317 159 0.476 0.45285714 0.53000000 0.3340336 1000 700 300
    ## 33              Sex & Marital Status             1 [3]  548 402 146 0.548 0.57428571 0.48666667 0.2664234 1000 700 300
    ## 34              Sex & Marital Status             2 [4]   92  67  25 0.092 0.09571429 0.08333333 0.2717391 1000 700 300
    ## 35              Sex & Marital Status             3 [2]  310 201 109 0.310 0.28714286 0.36333333 0.3516129 1000 700 300
    ## 36              Sex & Marital Status             4 [1]   50  30  20 0.050 0.04285714 0.06666667 0.4000000 1000 700 300
    ## 37                        Guarantors             1 [3]   52  42  10 0.052 0.06000000 0.03333333 0.1923077 1000 700 300
    ## 38                        Guarantors           2 [1,2]  948 658 290 0.948 0.94000000 0.96666667 0.3059072 1000 700 300
    ## 39       Duration in Current address             1 [1]  130  94  36 0.130 0.13428571 0.12000000 0.2769231 1000 700 300
    ## 40       Duration in Current address             2 [3]  149 106  43 0.149 0.15142857 0.14333333 0.2885906 1000 700 300
    ## 41       Duration in Current address             3 [4]  413 289 124 0.413 0.41285714 0.41333333 0.3002421 1000 700 300
    ## 42       Duration in Current address             4 [2]  308 211  97 0.308 0.30142857 0.32333333 0.3149351 1000 700 300
    ## 43     Most valuable available asset             1 [1]  282 222  60 0.282 0.31714286 0.20000000 0.2127660 1000 700 300
    ## 44     Most valuable available asset             2 [2]  232 161  71 0.232 0.23000000 0.23666667 0.3060345 1000 700 300
    ## 45     Most valuable available asset             3 [3]  332 230 102 0.332 0.32857143 0.34000000 0.3072289 1000 700 300
    ## 46     Most valuable available asset             4 [4]  154  87  67 0.154 0.12428571 0.22333333 0.4350649 1000 700 300
    ## 47                       Age (years)    1 [01 [19,26)]  187 108  79 0.187 0.15428571 0.26333333 0.4224599 1000 700 300
    ## 48                       Age (years)    2 [02 [26,35)]  349 238 111 0.349 0.34000000 0.37000000 0.3180516 1000 700 300
    ## 49                       Age (years)   3 [03 [35,Inf)]  464 354 110 0.464 0.50571429 0.36666667 0.2370690 1000 700 300
    ## 50                Concurrent Credits             1 [3]  814 590 224 0.814 0.84285714 0.74666667 0.2751843 1000 700 300
    ## 51                Concurrent Credits           2 [2,1]  186 110  76 0.186 0.15714286 0.25333333 0.4086022 1000 700 300
    ## 52                 Type of apartment             1 [2]  714 528 186 0.714 0.75428571 0.62000000 0.2605042 1000 700 300
    ## 53                 Type of apartment             2 [1]  179 109  70 0.179 0.15571429 0.23333333 0.3910615 1000 700 300
    ## 54                 Type of apartment             3 [3]  107  63  44 0.107 0.09000000 0.14666667 0.4112150 1000 700 300
    ## 55        No of Credits at this Bank           1 [3,2]  361 263  98 0.361 0.37571429 0.32666667 0.2714681 1000 700 300
    ## 56        No of Credits at this Bank           2 [1,4]  639 437 202 0.639 0.62428571 0.67333333 0.3161189 1000 700 300
    ## 57                        Occupation             1 [2]  200 144  56 0.200 0.20571429 0.18666667 0.2800000 1000 700 300
    ## 58                        Occupation             2 [3]  630 444 186 0.630 0.63428571 0.62000000 0.2952381 1000 700 300
    ## 59                        Occupation           3 [1,4]  170 112  58 0.170 0.16000000 0.19333333 0.3411765 1000 700 300
    ## 60                  No of dependents             1 [2]  155 109  46 0.155 0.15571429 0.15333333 0.2967742 1000 700 300
    ## 61                  No of dependents             2 [1]  845 591 254 0.845 0.84428571 0.84666667 0.3005917 1000 700 300
    ## 62                         Telephone             1 [2]  404 291 113 0.404 0.41571429 0.37666667 0.2797030 1000 700 300
    ## 63                         Telephone             2 [1]  596 409 187 0.596 0.58428571 0.62333333 0.3137584 1000 700 300
    ## 64                    Foreign Worker           1 [2,1] 1000 700 300 1.000 1.00000000 1.00000000 0.3000000 1000 700 300
    ##        dist.g     dist.b          woe            iv.b          iv.s       auc
    ## 1  0.49714286 0.15333333  1.176263223 0.4044104985393 0.66601150335 0.7077690
    ## 2  0.07000000 0.04666667  0.405465108 0.0094608525225 0.66601150335 0.7077690
    ## 3  0.23428571 0.35000000 -0.401391783 0.0464467634291 0.66601150335 0.7077690
    ## 4  0.19857143 0.45000000 -0.818098706 0.2056933888604 0.66601150335 0.7077690
    ## 5  0.11142857 0.03000000  1.312186389 0.1068494631015 0.26770659621 0.6241762
    ## 6  0.37714286 0.26666667  0.346624608 0.0382937662266 0.26770659621 0.6241762
    ## 7  0.46857143 0.57000000 -0.195947809 0.0198747062913 0.26770659621 0.6241762
    ## 8  0.04285714 0.13333333 -1.134979933 0.1026886605902 0.26770659621 0.6241762
    ## 9  0.34714286 0.16666667  0.733740578 0.1324227042295 0.29182985488 0.6265548
    ## 10 0.08571429 0.09333333 -0.085157808 0.0006488213969 0.29182985488 0.6265548
    ## 11 0.51571429 0.56333333 -0.088318617 0.0042056484275 0.29182985488 0.6265548
    ## 12 0.05142857 0.17666667 -1.234070835 0.1545526808248 0.29182985488 0.6265548
    ## 13 0.13428571 0.05666667  0.862783578 0.0669684396123 0.10753753988 0.5805190
    ## 14 0.31142857 0.27333333  0.130477955 0.0049705887671 0.10753753988 0.5805190
    ## 15 0.17571429 0.18000000 -0.024097552 0.0001032752211 0.10753753988 0.5805190
    ## 16 0.32857143 0.40333333 -0.205009097 0.0153268705898 0.10753753988 0.5805190
    ## 17 0.05000000 0.08666667 -0.550046337 0.0201683656870 0.10753753988 0.5805190
    ## 18 0.78571429 0.63000000  0.220873403 0.0343931441471 0.11944316082 0.5778571
    ## 19 0.21428571 0.37000000 -0.546192768 0.0850500166697 0.11944316082 0.5778571
    ## 20 0.13428571 0.05666667  0.862783578 0.0669684396123 0.19247255438 0.5987857
    ## 21 0.21571429 0.10666667  0.704246074 0.0767963575528 0.19247255438 0.5987857
    ## 22 0.09857143 0.11333333 -0.139551880 0.0020600515679 0.19247255438 0.5987857
    ## 23 0.55142857 0.72333333 -0.271357844 0.0466477056434 0.19247255438 0.5987857
    ## 24 0.19285714 0.13000000  0.394415272 0.0247918170922 0.08643363103 0.5808190
    ## 25 0.27000000 0.21333333  0.235566071 0.0133487440411 0.08643363103 0.5808190
    ## 26 0.33571429 0.34666667 -0.032103245 0.0003516069733 0.08643363103 0.5808190
    ## 27 0.05571429 0.07666667 -0.319230430 0.0066886375849 0.08643363103 0.5808190
    ## 28 0.14571429 0.23333333 -0.470820289 0.0412528253352 0.08643363103 0.5808190
    ## 29 0.14571429 0.11333333  0.251314428 0.0081378005348 0.02632209005 0.5433833
    ## 30 0.24142857 0.20666667  0.155466469 0.0054043106061 0.02632209005 0.5433833
    ## 31 0.16000000 0.15000000  0.064538521 0.0006453852114 0.02632209005 0.5433833
    ## 32 0.45285714 0.53000000 -0.157300289 0.0121345937020 0.02632209005 0.5433833
    ## 33 0.57428571 0.48666667  0.165547607 0.0145051236192 0.04467067763 0.5524238
    ## 34 0.09571429 0.08333333  0.138518934 0.0017149963274 0.04467067763 0.5524238
    ## 35 0.28714286 0.36333333 -0.235340835 0.0179307302520 0.04467067763 0.5524238
    ## 36 0.04285714 0.06666667 -0.441832752 0.0105198274352 0.04467067763 0.5524238
    ## 37 0.06000000 0.03333333  0.587786665 0.0156743110641 0.01642028045 0.5133333
    ## 38 0.94000000 0.96666667 -0.027973852 0.0007459693878 0.01642028045 0.5133333
    ## 39 0.13428571 0.12000000  0.112477983 0.0016068283347 0.00358877319 0.5161786
    ## 40 0.15142857 0.14333333  0.054941118 0.0004447614317 0.00358877319 0.5161786
    ## 41 0.41285714 0.41333333 -0.001152738 0.0000005489228 0.00358877319 0.5161786
    ## 42 0.30142857 0.32333333 -0.070150705 0.0015366344996 0.00358877319 0.5161786
    ## 43 0.31714286 0.20000000  0.461034959 0.0540069523708 0.11263826241 0.5853286
    ## 44 0.23000000 0.23666667 -0.028573372 0.0001904891496 0.11263826241 0.5853286
    ## 45 0.32857143 0.34000000 -0.034191365 0.0003907584543 0.11263826241 0.5853286
    ## 46 0.12428571 0.22333333 -0.586082361 0.0580500624351 0.11263826241 0.5853286
    ## 47 0.15428571 0.26333333 -0.534614486 0.0582984367772 0.10554156632 0.5857476
    ## 48 0.34000000 0.37000000 -0.084557388 0.0025367216408 0.10554156632 0.5857476
    ## 49 0.50571429 0.36666667  0.321518687 0.0447064079003 0.10554156632 0.5857476
    ## 50 0.84285714 0.74666667  0.121178625 0.0116562296099 0.05759207182 0.5480952
    ## 51 0.15714286 0.25333333 -0.477550835 0.0459358422124 0.05759207182 0.5480952
    ## 52 0.75428571 0.62000000  0.196051750 0.0263269492328 0.08539292555 0.5680619
    ## 53 0.15571429 0.23333333 -0.404445220 0.0313926528066 0.08539292555 0.5680619
    ## 54 0.09000000 0.14666667 -0.488352768 0.0276733235151 0.08539292555 0.5680619
    ## 55 0.37571429 0.32666667  0.139888693 0.0068612073292 0.01057079464 0.5245238
    ## 56 0.62428571 0.67333333 -0.075632363 0.0037095873130 0.01057079464 0.5245238
    ## 57 0.20571429 0.18666667  0.097163748 0.0018507380658 0.00848423370 0.5212381
    ## 58 0.63428571 0.62000000  0.022780028 0.0003254289762 0.00848423370 0.5212381
    ## 59 0.16000000 0.19333333 -0.189242000 0.0063080666546 0.00848423370 0.5212381
    ## 60 0.15571429 0.15333333  0.015408625 0.0000366872032 0.00004339223 0.5011905
    ## 61 0.84428571 0.84666667 -0.002816110 0.0000067050238 0.00004339223 0.5011905
    ## 62 0.41571429 0.37666667  0.098637588 0.0038515629628 0.00637760503 0.5195238
    ## 63 0.58428571 0.62333333 -0.064691321 0.0025260420659 0.00637760503 0.5195238
    ## 64 1.00000000 1.00000000  0.000000000 0.0000000000000 0.00000000000        NA
    ## 
    ## $info
    ## data frame with 0 columns and 0 rows

After additional binning based on above criteria and running again the
bivariate analysis, we see that risk factor `Foreign Worker` has only
one value and it can be discarded from the further analysis.

``` r
loans <- loans[, !names(loans)%in%"Foreign Worker"]
```
