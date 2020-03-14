
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
#>    category betas estimate     se
#>    <chr>    <int>    <dbl>  <dbl>
#>  1 `2`          1 -1.10    0.148 
#>  2 `2`          2 -0.132   0.149 
#>  3 `2`          3  0.452   0.139 
#>  4 `2`          4 -0.312   0.138 
#>  5 `3`          1  0.478   0.0779
#>  6 `3`          2 -0.00282 0.0788
#>  7 `3`          3 -0.0961  0.0807
#>  8 `3`          4 -0.114   0.0684
#>  9 `4`          1 -0.915   0.132 
#> 10 `4`          2 -0.140   0.131 
#> 11 `4`          3  0.569   0.121 
#> 12 `4`          4 -0.166   0.120 
#> 13 `5`          1  0.624   0.0739
#> 14 `5`          2 -0.161   0.0743
#> 15 `5`          3 -0.0458  0.0774
#> 16 `5`          4  0.0931  0.0653
#> 17 `6`          1 -0.901   0.132 
#> 18 `6`          2 -0.393   0.132 
#> 19 `6`          3  0.513   0.130 
#> 20 `6`          4 -0.00985 0.124

## predicted probability
prob <- predict(fit)
head(prob)
#>           [,1]       [,2]      [,3]       [,4]      [,5]       [,6]
#> [1,] 0.1780091 0.05920858 0.2871146 0.07126296 0.3321263 0.07227851
#> [2,] 0.1894582 0.06355165 0.2470729 0.09861256 0.3154416 0.08586314
#> [3,] 0.1624737 0.08492369 0.2380444 0.11484718 0.2895647 0.11014634
#> [4,] 0.1827376 0.04451105 0.2630886 0.06196287 0.3742289 0.07347090
#> [5,] 0.1624737 0.08492369 0.2380444 0.11484718 0.2895647 0.11014634
#> [6,] 0.1780091 0.05920858 0.2871146 0.07126296 0.3321263 0.07227851
```
