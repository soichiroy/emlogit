
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emlogit

<!-- badges: start -->

[![R build
status](https://github.com/soichiroy/emlogit/workflows/R-CMD-check/badge.svg)](https://github.com/soichiroy/emlogit/actions)
<!-- badges: end -->

`emlogit` is a **R** package that implements the Expectation and
Conditional-Maximization (ECM) algorithm for the multinomial logistic
regression.



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
#>         category     betas estimate    se
#> 1  choicechoice2 intercept  -1.1007 0.148
#> 2  choicechoice2   college  -0.1320 0.149
#> 3  choicechoice2      hsg2   0.4519 0.139
#> 4  choicechoice2     coml5  -0.3114 0.138
#> 5  choicechoice3 intercept   0.4781 0.078
#> 6  choicechoice3   college  -0.0028 0.079
#> 7  choicechoice3      hsg2  -0.0961 0.081
#> 8  choicechoice3     coml5  -0.1136 0.068
#> 9  choicechoice4 intercept  -0.9154 0.132
#> 10 choicechoice4   college  -0.1400 0.131
#> 11 choicechoice4      hsg2   0.5685 0.121
#> 12 choicechoice4     coml5  -0.1660 0.120
#> 13 choicechoice5 intercept   0.6237 0.074
#> 14 choicechoice5   college  -0.1612 0.074
#> 15 choicechoice5      hsg2  -0.0458 0.077
#> 16 choicechoice5     coml5   0.0932 0.065
#> 17 choicechoice6 intercept  -0.9013 0.132
#> 18 choicechoice6   college  -0.3928 0.132
#> 19 choicechoice6      hsg2   0.5126 0.130
#> 20 choicechoice6     coml5  -0.0098 0.124

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
#>    category     betas estimate     se
#> 1       JCP intercept  -0.5997 0.9744
#> 2       JCP    gender   0.0777 0.3161
#> 3       JCP education   0.0314 0.1616
#> 4       JCP       age  -0.0082 0.0129
#> 5       NFP intercept   0.5641 0.6982
#> 6       NFP    gender  -0.1651 0.2284
#> 7       NFP education  -0.0198 0.1220
#> 8       NFP       age  -0.0057 0.0094
#> 9       SKG intercept  -0.2141 0.7655
#> 10      SKG    gender  -0.0710 0.2494
#> 11      SKG education   0.0578 0.1296
#> 12      SKG       age  -0.0019 0.0103

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
