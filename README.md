
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
#>  2 choicechoice2 college   -0.133   0.149 
#>  3 choicechoice2 hsg2       0.452   0.139 
#>  4 choicechoice2 coml5     -0.312   0.138 
#>  5 choicechoice3 intercept  0.479   0.0779
#>  6 choicechoice3 college   -0.00330 0.0788
#>  7 choicechoice3 hsg2      -0.0963  0.0807
#>  8 choicechoice3 coml5     -0.114   0.0684
#>  9 choicechoice4 intercept -0.915   0.132 
#> 10 choicechoice4 college   -0.141   0.131 
#> 11 choicechoice4 hsg2       0.568   0.121 
#> 12 choicechoice4 coml5     -0.166   0.120 
#> 13 choicechoice5 intercept  0.624   0.0739
#> 14 choicechoice5 college   -0.162   0.0743
#> 15 choicechoice5 hsg2      -0.0460  0.0774
#> 16 choicechoice5 coml5      0.0931  0.0653
#> 17 choicechoice6 intercept -0.901   0.132 
#> 18 choicechoice6 college   -0.393   0.132 
#> 19 choicechoice6 hsg2       0.512   0.130 
#> 20 choicechoice6 coml5     -0.00993 0.124

## predicted probability
prob <- predict(fit)
head(prob)
#>           [,1]       [,2]      [,3]       [,4]      [,5]       [,6]
#> [1,] 0.1779406 0.05924217 0.2871347 0.07127576 0.3321287 0.07227805
#> [2,] 0.1895030 0.06352991 0.2470618 0.09860203 0.3154403 0.08586304
#> [3,] 0.1624314 0.08495275 0.2380504 0.11485926 0.2895611 0.11014510
#> [4,] 0.1826826 0.04453181 0.2631067 0.06197228 0.3742354 0.07347117
#> [5,] 0.1624314 0.08495275 0.2380504 0.11485926 0.2895611 0.11014510
#> [6,] 0.1779406 0.05924217 0.2871347 0.07127576 0.3321287 0.07227805
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
#>  2 JCP      gender     0.0777  0.316  
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
#> [1,] 0.3557916 0.2880367 0.2291345 0.1270371
#> [2,] 0.3242629 0.3295629 0.2289084 0.1172659
#> [3,] 0.3308956 0.2925249 0.2340318 0.1425478
#> [4,] 0.3265660 0.2953230 0.2327214 0.1453896
#> [5,] 0.3181369 0.2765431 0.2544981 0.1508219
#> [6,] 0.2835531 0.3340386 0.2391526 0.1432558
```
