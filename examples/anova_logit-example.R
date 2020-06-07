



##
## Example
##

require(dplyr)
require(purrr)
require(stringr)
data(japan, package = "MNP")


japan <- japan %>%
  mutate(n = LDP + NFP + SKG + JCP) %>%
  mutate(age_bin = ntile(age, 10))

xx <- al_data(
  formula = LDP | n ~  gender + age_bin + education,
  data = japan,
  adaptive_weight = TRUE
)


## fit without regularization
fit <- anova_logit(
  y = xx$y,
  X = xx$X,
  trials = xx$trials,
  option = list(regularize = FALSE, pvec = xx$pvec)
)


coef_res <- fit$coef
names(coef_res) <- c("intercept", unlist(map(xx$nj, ~names(.x))))
sum(coef_res[str_detect(names(coef_res), "gender")])
sum(coef_res[str_detect(names(coef_res), "education")])
sum(coef_res[str_detect(names(coef_res), "age_bin")])

# debugonce(al_em_run)
set.seed(1235)
fit <- anova_logit(
  y = xx$y,
  X = xx$X,
  trials = xx$trials,
  option = list(regularize = TRUE, pvec = xx$pvec, lambda = 3, wj = NULL) # xx$wj)
)

coef_res <- fit$coef
names(coef_res) <- c("intercept", unlist(map(xx$nj, ~names(.x))))
sum(coef_res[str_detect(names(coef_res), "gender")])
sum(coef_res[str_detect(names(coef_res), "age_bin")])
sum(coef_res[str_detect(names(coef_res), "education")])


## fit with regularization
require(future)
require(furrr)
require(ggplot2)

lambda_vec <- exp(seq(-3, 3, by = 0.1))

## setup parallel computation
plan(multiprocess)
set.seed(1234)
fit_reg <- furrr::future_map(
  lambda_vec, ~ anova_logit(
    y = xx$y,
    X = xx$X,
    trials = xx$trials,
    option = list(regularize = TRUE, pvec = xx$pvec, lambda = .x, wj = xx$wj)
  ), .progress = TRUE
)


## extract BIC
purrr::map_dbl(fit_reg, ~.x$BIC) %>%
  rlang::set_names(lambda_vec) %>%
  enframe() %>%
  ggplot(aes(x = as.numeric(name), y = value)) +
  geom_line() +
  geom_point()

## solution path
purrr::map(fit_reg, ~.x$coef) %>%
  do.call(rbind, .) %>%
  matplot(., type = 'l')


BIC <- purrr::map_dbl(fit_reg, ~.x$BIC)
coef_res <- round(fit_reg[[which(BIC == min(BIC))]]$coef, 3)
names(coef_res) <- c("intercept", unlist(map(xx$nj, ~names(.x))))
coef_res


par(mfrow = c(1, 2))
hist(fit_reg[[which(BIC == min(BIC))]]$fitted)
hist(xx$y/xx$trials)
