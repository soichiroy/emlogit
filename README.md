
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emlogit

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/soichiroy/emlogit.svg?branch=master)](https://travis-ci.com/soichiroy/emlogit)
[![R build
status](https://github.com/soichiroy/emlogit/workflows/R-CMD-check/badge.svg)](https://github.com/soichiroy/emlogit/actions)
<!-- badges: end -->

`emlogit` is a **R** package that implements the Expectation and
Conditional-Maximization (ECM) algorithm for the multinomial logistic
regression.

## Table of Contents

| [Installation](#installation) | [Examples](#examples) | [References](#references) |
| ----------------------------- | --------------------- | ------------------------- |

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("soichiroy/emlogit")
```

## Examples

### Categorical outcome

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
#> [90m# A tibble: 20 x 4[39m
#>    category      betas     estimate     se
#>    [3m[90m<chr>[39m[23m         [3m[90m<chr>[39m[23m        [3m[90m<dbl>[39m[23m  [3m[90m<dbl>[39m[23m
#> [90m 1[39m choicechoice2 intercept -[31m1[39m[31m.[39m[31m10[39m    0.148 
#> [90m 2[39m choicechoice2 college   -[31m0[39m[31m.[39m[31m132[39m   0.149 
#> [90m 3[39m choicechoice2 hsg2       0.452   0.139 
#> [90m 4[39m choicechoice2 coml5     -[31m0[39m[31m.[39m[31m311[39m   0.138 
#> [90m 5[39m choicechoice3 intercept  0.478   0.077[4m9[24m
#> [90m 6[39m choicechoice3 college   -[31m0[39m[31m.[39m[31m00[39m[31m2[4m8[24m[4m1[24m[39m 0.078[4m8[24m
#> [90m 7[39m choicechoice3 hsg2      -[31m0[39m[31m.[39m[31m0[39m[31m96[4m1[24m[39m  0.080[4m7[24m
#> [90m 8[39m choicechoice3 coml5     -[31m0[39m[31m.[39m[31m114[39m   0.068[4m4[24m
#> [90m 9[39m choicechoice4 intercept -[31m0[39m[31m.[39m[31m915[39m   0.132 
#> [90m10[39m choicechoice4 college   -[31m0[39m[31m.[39m[31m140[39m   0.131 
#> [90m11[39m choicechoice4 hsg2       0.569   0.121 
#> [90m12[39m choicechoice4 coml5     -[31m0[39m[31m.[39m[31m166[39m   0.120 
#> [90m13[39m choicechoice5 intercept  0.624   0.073[4m9[24m
#> [90m14[39m choicechoice5 college   -[31m0[39m[31m.[39m[31m161[39m   0.074[4m3[24m
#> [90m15[39m choicechoice5 hsg2      -[31m0[39m[31m.[39m[31m0[39m[31m45[4m8[24m[39m  0.077[4m4[24m
#> [90m16[39m choicechoice5 coml5      0.093[4m2[24m  0.065[4m3[24m
#> [90m17[39m choicechoice6 intercept -[31m0[39m[31m.[39m[31m901[39m   0.132 
#> [90m18[39m choicechoice6 college   -[31m0[39m[31m.[39m[31m393[39m   0.132 
#> [90m19[39m choicechoice6 hsg2       0.513   0.130 
#> [90m20[39m choicechoice6 coml5     -[31m0[39m[31m.[39m[31m00[39m[31m9[4m8[24m[4m3[24m[39m 0.124

## predicted probability
prob <- predict(fit)
```

``` r
head(prob) %>%
  knitr::kable(digit = 3)
```

| choicechoice1 | choicechoice2 | choicechoice3 | choicechoice4 | choicechoice5 | choicechoice6 |
| ------------: | ------------: | ------------: | ------------: | ------------: | ------------: |
|         0.178 |         0.059 |         0.287 |         0.071 |         0.332 |         0.072 |
|         0.189 |         0.064 |         0.247 |         0.099 |         0.315 |         0.086 |
|         0.162 |         0.085 |         0.238 |         0.115 |         0.290 |         0.110 |
|         0.183 |         0.045 |         0.263 |         0.062 |         0.374 |         0.073 |
|         0.162 |         0.085 |         0.238 |         0.115 |         0.290 |         0.110 |
|         0.178 |         0.059 |         0.287 |         0.071 |         0.332 |         0.072 |

### Multinomial outcome

`japan` data from `MNP` package.

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

# prepare datainput
Y <- japan %>% select(LDP, NFP, SKG, JCP) %>% data.matrix()
X <- japan %>% select(gender, education, age) %>% data.matrix()

set.seed(1234)
fit <- emlogit(Y = Y, X = X)
summary(fit)
#> [90m# A tibble: 12 x 4[39m
#>    category betas     estimate      se
#>    [3m[90m<chr>[39m[23m    [3m[90m<chr>[39m[23m        [3m[90m<dbl>[39m[23m   [3m[90m<dbl>[39m[23m
#> [90m 1[39m JCP      intercept -[31m0[39m[31m.[39m[31m600[39m   0.974  
#> [90m 2[39m JCP      gender     0.077[4m7[24m  0.316  
#> [90m 3[39m JCP      education  0.031[4m4[24m  0.162  
#> [90m 4[39m JCP      age       -[31m0[39m[31m.[39m[31m00[39m[31m8[4m2[24m[4m3[24m[39m 0.012[4m9[24m 
#> [90m 5[39m NFP      intercept  0.564   0.698  
#> [90m 6[39m NFP      gender    -[31m0[39m[31m.[39m[31m165[39m   0.228  
#> [90m 7[39m NFP      education -[31m0[39m[31m.[39m[31m0[39m[31m19[4m8[24m[39m  0.122  
#> [90m 8[39m NFP      age       -[31m0[39m[31m.[39m[31m00[39m[31m5[4m6[24m[4m7[24m[39m 0.009[4m4[24m[4m3[24m
#> [90m 9[39m SKG      intercept -[31m0[39m[31m.[39m[31m214[39m   0.766  
#> [90m10[39m SKG      gender    -[31m0[39m[31m.[39m[31m0[39m[31m71[4m0[24m[39m  0.249  
#> [90m11[39m SKG      education  0.057[4m8[24m  0.130  
#> [90m12[39m SKG      age       -[31m0[39m[31m.[39m[31m00[39m[31m1[4m8[24m[4m9[24m[39m 0.010[4m3[24m

pred <- predict(fit)
```

``` r
head(pred) %>%
  knitr::kable(digit = 3)
```

|   LDP |   NFP |   SKG |   JCP |
| ----: | ----: | ----: | ----: |
| 0.356 | 0.288 | 0.229 | 0.127 |
| 0.324 | 0.330 | 0.229 | 0.117 |
| 0.331 | 0.293 | 0.234 | 0.143 |
| 0.327 | 0.295 | 0.233 | 0.145 |
| 0.318 | 0.277 | 0.254 | 0.151 |
| 0.284 | 0.334 | 0.239 | 0.143 |

### Binary outcome

Since the binomial outcome is a special case of multinomial outcomes,
`emlogit` can handle the binomial outcome.

``` r
set.seed(1234)

## generate X and pi(X)
betas <- rnorm(4)
X     <- MASS::mvrnorm(5000, mu = rep(0, length(betas)), Sigma = diag(length(betas)))

prob  <- 1 / (1 + exp(- 0.5 - X %*% betas))

## generate outcome
ybin <- rbinom(n = 5000, size = 1, prob = prob)
Y    <- model.matrix(~ factor(ybin) - 1)

## fit
fit <- emlogit(Y = Y, X = X)


## check
betas <- c(0.5, betas)
cbind(true = betas, estimate = summary(fit)$estimate) %>%
  knitr::kable(digit = 3)
```

|    true | estimate |
| ------: | -------: |
|   0.500 |    0.486 |
| \-1.207 |  \-1.258 |
|   0.277 |    0.303 |
|   1.084 |    1.040 |
| \-2.346 |  \-2.316 |

## References

The ECM algorithm used in this package utilizes the Polya-Gamma
augmentation scheme originally developed by Polson et al. (2013).
Applications to the multinomial outcome in the context of deriving the
EM algorithm based on Polya-Gamma representation can be found in Durante
et al. (2019) and Goplerud (2019), among others.

  - Durante, D., Canale, A., & Rigon, T. (2019). [A nested
    expectation–maximization algorithm for latent class models with
    covariates.](https://doi.org/10.1016/j.spl.2018.10.015) *Statistics
    & Probability Letters,* 146, 97-103.
  - Goplerud, M. (2019). [A Multinomial Framework for Ideal Point
    Estimation.](https://doi.org/10.1017/pan.2018.31) *Political
    Analysis,* 27(1), 69-89.
  - Polson, N. G., Scott, J. G., & Windle, J. (2013). [Bayesian
    inference for logistic models using Pólya–Gamma latent
    variables.](https://doi.org/10.1080/01621459.2013.829001) *Journal
    of the American Statistical Association,* 108(504), 1339-1349.
