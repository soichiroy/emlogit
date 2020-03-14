
<!-- README.md is generated from README.Rmd. Please edit that file -->
emlogit
=======

<!-- badges: start -->
<!-- badges: end -->
`emlogit` implements the EM algorithm for the multinomial logistic regression.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sou412/emlogit")
```

Example
-------

`Car` data from `mlogit` package.

``` r
require(emlogit)
require(dplyr)
data(Car, package = 'mlogit')

## prepare data: only allow for indiviual specific covariates
y <- Car %>% select(choice)
Y <- model.matrix(~choice-1, data = y)
X <- Car %>% select(college, hsg2, coml5) %>% data.matrix()
```

``` r
## fit
fit <- emlogit(Y = Y, X = X)
summary(fit)
#> # A tibble: 20 x 4
#>    category      betas     estimate     se
#>    <chr>         <chr>        <dbl>  <dbl>
#>  1 choicechoice2 intercept -1.10    0.148 
#>  2 choicechoice2 college   -0.132   0.149 
#>  3 choicechoice2 hsg2       0.452   0.139 
#>  4 choicechoice2 coml5     -0.311   0.138 
#>  5 choicechoice3 intercept  0.478   0.0779
#>  6 choicechoice3 college   -0.00283 0.0788
#>  7 choicechoice3 hsg2      -0.0961  0.0807
#>  8 choicechoice3 coml5     -0.114   0.0684
#>  9 choicechoice4 intercept -0.915   0.132 
#> 10 choicechoice4 college   -0.140   0.131 
#> 11 choicechoice4 hsg2       0.569   0.121 
#> 12 choicechoice4 coml5     -0.166   0.120 
#> 13 choicechoice5 intercept  0.624   0.0739
#> 14 choicechoice5 college   -0.161   0.0743
#> 15 choicechoice5 hsg2      -0.0458  0.0774
#> 16 choicechoice5 coml5      0.0932  0.0653
#> 17 choicechoice6 intercept -0.901   0.132 
#> 18 choicechoice6 college   -0.393   0.132 
#> 19 choicechoice6 hsg2       0.513   0.130 
#> 20 choicechoice6 coml5     -0.00981 0.124

## predicted probability
prob <- predict(fit)
```

|  choicechoice1|  choicechoice2|  choicechoice3|  choicechoice4|  choicechoice5|  choicechoice6|
|--------------:|--------------:|--------------:|--------------:|--------------:|--------------:|
|          0.178|          0.059|          0.287|          0.071|          0.332|          0.072|
|          0.189|          0.064|          0.247|          0.099|          0.315|          0.086|
|          0.162|          0.085|          0.238|          0.115|          0.290|          0.110|
|          0.183|          0.045|          0.263|          0.062|          0.374|          0.073|
|          0.162|          0.085|          0.238|          0.115|          0.290|          0.110|
|          0.178|          0.059|          0.287|          0.071|          0.332|          0.072|

``` r
## multinomial outcome 
data(japan, package = "MNP")
head(japan)
#>   LDP NFP SKG JCP gender education age
#> 1  80  75  80   0   male         1  75
#> 2  75  80  50  20 female         1  64
#> 3 100  25 100   0   male         2  56
#> 4  75  50  25  50   male         2  52
#> 5  75  50  50   0   male         4  52
#> 6  50  75  50  50 female         3  31

Y <- japan %>% select(LDP, NFP, SKG, JCP) %>% data.matrix()
X <- japan %>% select(gender, education, age) %>% data.matrix()

set.seed(1234)
fit <- emlogit(Y = Y, X = X)
summary(fit)
#> # A tibble: 12 x 4
#>    category betas     estimate      se
#>    <chr>    <chr>        <dbl>   <dbl>
#>  1 JCP      intercept -0.600   0.974  
#>  2 JCP      gender     0.0778  0.316  
#>  3 JCP      education  0.0314  0.162  
#>  4 JCP      age       -0.00823 0.0129 
#>  5 NFP      intercept  0.564   0.698  
#>  6 NFP      gender    -0.165   0.228  
#>  7 NFP      education -0.0198  0.122  
#>  8 NFP      age       -0.00567 0.00943
#>  9 SKG      intercept -0.214   0.766  
#> 10 SKG      gender    -0.0710  0.249  
#> 11 SKG      education  0.0578  0.130  
#> 12 SKG      age       -0.00189 0.0103

pred <- predict(fit)
```

|  choicechoice1|  choicechoice2|  choicechoice3|  choicechoice4|  choicechoice5|  choicechoice6|
|--------------:|--------------:|--------------:|--------------:|--------------:|--------------:|
|          0.178|          0.059|          0.287|          0.071|          0.332|          0.072|
|          0.189|          0.064|          0.247|          0.099|          0.315|          0.086|
|          0.162|          0.085|          0.238|          0.115|          0.290|          0.110|
|          0.183|          0.045|          0.263|          0.062|          0.374|          0.073|
|          0.162|          0.085|          0.238|          0.115|          0.290|          0.110|
|          0.178|          0.059|          0.287|          0.071|          0.332|          0.072|
