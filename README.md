
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
#>  6 choicechoice3 college   -0.00280 0.0788
#>  7 choicechoice3 hsg2      -0.0962  0.0807
#>  8 choicechoice3 coml5     -0.114   0.0684
#>  9 choicechoice4 intercept -0.915   0.132 
#> 10 choicechoice4 college   -0.140   0.131 
#> 11 choicechoice4 hsg2       0.568   0.121 
#> 12 choicechoice4 coml5     -0.166   0.120 
#> 13 choicechoice5 intercept  0.624   0.0739
#> 14 choicechoice5 college   -0.161   0.0743
#> 15 choicechoice5 hsg2      -0.0459  0.0774
#> 16 choicechoice5 coml5      0.0932  0.0653
#> 17 choicechoice6 intercept -0.901   0.132 
#> 18 choicechoice6 college   -0.393   0.132 
#> 19 choicechoice6 hsg2       0.513   0.130 
#> 20 choicechoice6 coml5     -0.00980 0.124

## predicted probability
prob <- predict(fit)
head(prob)
#>           [,1]       [,2]      [,3]       [,4]      [,5]       [,6]
#> [1,] 0.1780051 0.05921078 0.2871155 0.07126386 0.3321263 0.07227843
#> [2,] 0.1894495 0.06355830 0.2470738 0.09861463 0.3154408 0.08586293
#> [3,] 0.1624775 0.08491975 0.2380444 0.11484606 0.2895657 0.11014664
#> [4,] 0.1827245 0.04451819 0.2630917 0.06196543 0.3742293 0.07347089
#> [5,] 0.1624775 0.08491975 0.2380444 0.11484606 0.2895657 0.11014664
#> [6,] 0.1780051 0.05921078 0.2871155 0.07126386 0.3321263 0.07227843
```

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
head(pred)
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.3557927 0.2880360 0.2291343 0.1270370
#> [2,] 0.3242640 0.3295621 0.2289081 0.1172658
#> [3,] 0.3308962 0.2925244 0.2340316 0.1425478
#> [4,] 0.3265665 0.2953226 0.2327212 0.1453896
#> [5,] 0.3181381 0.2765421 0.2544978 0.1508219
#> [6,] 0.2835535 0.3340382 0.2391525 0.1432558
```
