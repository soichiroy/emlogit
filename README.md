
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
#>  1 choicechoice2 intercept  -1.08   0.148 
#>  2 choicechoice2 college    -0.151  0.149 
#>  3 choicechoice2 hsg2        0.451  0.139 
#>  4 choicechoice2 coml5      -0.317  0.139 
#>  5 choicechoice3 intercept   0.486  0.0779
#>  6 choicechoice3 college    -0.0118 0.0787
#>  7 choicechoice3 hsg2       -0.0964 0.0807
#>  8 choicechoice3 coml5      -0.116  0.0684
#>  9 choicechoice4 intercept  -0.905  0.132 
#> 10 choicechoice4 college    -0.151  0.131 
#> 11 choicechoice4 hsg2        0.568  0.121 
#> 12 choicechoice4 coml5      -0.169  0.120 
#> 13 choicechoice5 intercept   0.631  0.0739
#> 14 choicechoice5 college    -0.169  0.0743
#> 15 choicechoice5 hsg2       -0.0461 0.0774
#> 16 choicechoice5 coml5       0.0912 0.0653
#> 17 choicechoice6 intercept  -0.894  0.132 
#> 18 choicechoice6 college    -0.400  0.132 
#> 19 choicechoice6 hsg2        0.512  0.130 
#> 20 choicechoice6 coml5      -0.0118 0.124

## predicted probability
prob <- predict(fit)
head(prob)
#>      choicechoice1 choicechoice2 choicechoice3 choicechoice4 choicechoice5
#> [1,]     0.1768097    0.05976475     0.2874775    0.07149129     0.3321743
#> [2,]     0.1900410    0.06318904     0.2469693    0.09846840     0.3154583
#> [3,]     0.1613588    0.08566520     0.2382378    0.11513754     0.2894954
#> [4,]     0.1818907    0.04477226     0.2633908    0.06211566     0.3743443
#> [5,]     0.1613588    0.08566520     0.2382378    0.11513754     0.2894954
#> [6,]     0.1768097    0.05976475     0.2874775    0.07149129     0.3321743
#>      choicechoice6
#> [1,]    0.07228244
#> [2,]    0.08587396
#> [3,]    0.11010526
#> [4,]    0.07348626
#> [5,]    0.11010526
#> [6,]    0.07228244
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
#>            LDP       NFP       SKG       JCP
#> [1,] 0.3557931 0.2880357 0.2291342 0.1270370
#> [2,] 0.3242650 0.3295614 0.2289079 0.1172657
#> [3,] 0.3308963 0.2925243 0.2340316 0.1425478
#> [4,] 0.3265666 0.2953226 0.2327212 0.1453896
#> [5,] 0.3181383 0.2765420 0.2544978 0.1508219
#> [6,] 0.2835540 0.3340379 0.2391523 0.1432557
```
