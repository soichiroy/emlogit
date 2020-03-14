
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emlogit

<!-- badges: start -->

<!-- badges: end -->

`emlogit` implements the EM algorithm for the multinomial logistic
regression.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sou412/emlogit")
```

## Example

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
#>    category betas estimate     se
#>    <chr>    <int>    <dbl>  <dbl>
#>  1 `2`          1 -1.10    0.164 
#>  2 `2`          2 -0.132   0.166 
#>  3 `2`          3  0.452   0.162 
#>  4 `2`          4 -0.311   0.183 
#>  5 `3`          1  0.478   0.152 
#>  6 `3`          2 -0.00288 0.152 
#>  7 `3`          3 -0.0961  0.142 
#>  8 `3`          4 -0.114   0.169 
#>  9 `4`          1 -0.915   0.0791
#> 10 `4`          2 -0.140   0.0832
#> 11 `4`          3  0.569   0.0846
#> 12 `4`          4 -0.166   0.0840
#> 13 `5`          1  0.624   0.120 
#> 14 `5`          2 -0.161   0.124 
#> 15 `5`          3 -0.0458  0.115 
#> 16 `5`          4  0.0932  0.143 
#> 17 `6`          1 -0.901   0.0924
#> 18 `6`          2 -0.393   0.106 
#> 19 `6`          3  0.513   0.112 
#> 20 `6`          4 -0.00982 0.140

## predicted probability
prob <- predict(fit)
head(prob)
#>           [,1]       [,2]      [,3]       [,4]      [,5]       [,6]
#> [1,] 0.1779955 0.05921515 0.2871185 0.07126563 0.3321268 0.07227847
#> [2,] 0.1894498 0.06355695 0.2470744 0.09861424 0.3154415 0.08586305
#> [3,] 0.1624636 0.08492834 0.2380472 0.11484945 0.2895652 0.11014625
#> [4,] 0.1827181 0.04452006 0.2630940 0.06196653 0.3742302 0.07347100
#> [5,] 0.1624636 0.08492834 0.2380472 0.11484945 0.2895652 0.11014625
#> [6,] 0.1779955 0.05921515 0.2871185 0.07126563 0.3321268 0.07227847
```
