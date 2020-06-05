



##
## Example
##

require(dplyr)
data(japan, package = "MNP")


japan <- japan %>%
  mutate(n = LDP + NFP + SKG + JCP) %>%
  mutate(age_bin = ntile(age, 10))

# debugonce(al_data)
xx <- al_data(
  formula = LDP | n ~ gender + education + age_bin,
  data = japan
)


## fit without regularization
fit <- anova_logit(
  y = xx$y,
  X = xx$X,
  trials = xx$trials,
  option = list(regularize = FALSE, pvec = xx$pvec)
)

## fit with regularization
require(future)
require(furrr)
require(ggplot2)

lambda_vec <- exp(seq(-1, 2, by = 0.05))

## setup parallel computation
plan(multiprocess)
fit_reg <- furrr::future_map(
  lambda_vec, ~ anova_logit(
    y = xx$y,
    X = xx$X,
    trials = xx$trials,
    option = list(regularize = TRUE, pvec = xx$pvec, lambda = .x, wj = xx$wj)
  )
)


## extract BIC
purrr::map_dbl(fit_reg, ~.x$BIC) %>%
  rlang::set_names(lambda_vec) %>%
  enframe() %>%
  ggplot(aes(x = as.numeric(name), y = value)) +
  geom_line() +
  geom_point()

purrr::map(fit_reg, ~.x$coef) %>%
  do.call(rbind, .) %>%
  matplot(., type = 'l')


BIC <- purrr::map_dbl(fit_reg, ~.x$BIC)
coef_res <- round(fit_reg[[which(BIC == min(BIC))]]$coef, 3)

names(coef_res) <- c("intercept", unlist(map(xx$nj, ~names(.x))))
