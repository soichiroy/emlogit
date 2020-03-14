
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
#> Loading required package: emlogit
require(dplyr)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
data(Car, package = 'mlogit')

## prepare data: only allow for indiviual specific covariates
y <- Car %>% select(choice)
Y <- model.matrix(~choice-1, data = y)
X <- Car %>% select(college, hsg2, coml5) %>% data.matrix()

## fit
fit <- emlogit(Y = Y, X = X)
summary(fit)
#> # A tibble: 20 x 4
#>    category betas estimate     se
#>    <chr>    <int>    <dbl>  <dbl>
#>  1 `2`          1 -1.10    0.117 
#>  2 `2`          2 -0.132   0.122 
#>  3 `2`          3  0.452   0.148 
#>  4 `2`          4 -0.311   0.122 
#>  5 `3`          1  0.478   0.0742
#>  6 `3`          2 -0.00274 0.0761
#>  7 `3`          3 -0.0962  0.0809
#>  8 `3`          4 -0.114   0.0749
#>  9 `4`          1 -0.915   0.194 
#> 10 `4`          2 -0.140   0.207 
#> 11 `4`          3  0.568   0.273 
#> 12 `4`          4 -0.166   0.206 
#> 13 `5`          1  0.624   0.155 
#> 14 `5`          2 -0.161   0.152 
#> 15 `5`          3 -0.0459  0.180 
#> 16 `5`          4  0.0932  0.135 
#> 17 `6`          1 -0.901   0.122 
#> 18 `6`          2 -0.393   0.121 
#> 19 `6`          3  0.513   0.0792
#> 20 `6`          4 -0.00983 0.0680
```
