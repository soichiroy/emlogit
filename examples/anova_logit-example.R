



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
round(coef_res, 3)


hist(fit$fitted)

plot(fit$fitted, 
    japan %>% group_by(gender, education, age_bin) %>% 
      mutate(prop = LDP / n) %>% 
      pull(prop),
      pch = 16,
      xlim = c(0, 1), 
      ylim = c(0, 1),
      xlab = "Fitted (Pr voting for LDP)", 
      ylab = "Data (Pr voting for LDP)"
)

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

lambda_vec <- exp(seq(-3.1, 3, by = 0.1))

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

coef_path <- purrr::map_dfr(fit_reg, ~ tibble(
  variable = c("intercept", unlist(map(xx$nj, ~names(.x)))),
  coef     = .x$coef), .id = 'lambda') %>% 
  mutate(var_group = case_when(
    variable == 'intercept' ~ 'intercept',
    str_detect(variable, "gender") ~ 'gender',
    str_detect(variable, "age") ~ 'age',
    str_detect(variable, "education") ~ 'education'
  )) %>% 
  mutate(lambda = as.numeric(lambda))
  
  
BIC <- purrr::map_dbl(fit_reg, ~.x$BIC)

coef_path   %>% 
  ggplot(aes(x = lambda, y = coef)) + 
  geom_line(aes(group = variable, color = var_group)) +
  theme_bw() + 
  geom_vline(xintercept = which(BIC == min(BIC)), linetype = 'dashed',
    color = 'gray')
  
  


BIC <- purrr::map_dbl(fit_reg, ~.x$BIC)
coef_res <- round(fit_reg[[which(BIC == min(BIC))]]$coef, 3)
names(coef_res) <- c("intercept", unlist(map(xx$nj, ~names(.x))))
coef_res


par(mfrow = c(1, 2))
hist(fit_reg[[which(BIC == min(BIC))]]$fitted)
hist(xx$y/xx$trials)

japan_summary <- japan %>% 
  mutate(fitted = fit_reg[[which(BIC == min(BIC))]]$fitted) %>% 
  group_by(gender, education, age_bin) %>% 
  summarise(prop = mean(fitted))

par(mfrow = c(1, 2))
plot(japan_summary$prop , 
    japan %>% group_by(gender, education, age_bin) %>% 
      summarise(prop = sum(LDP) / sum(n)) %>% 
      ungroup() %>% 
      pull(prop),
      pch = 16,
      xlim = c(0, 1), 
      ylim = c(0, 1),
      xlab = "Fitted (Pr voting for LDP)", 
      ylab = "Data (Pr voting for LDP)"
)
abline(0, 1)

qqplot(japan_summary$prop , 
    japan %>% group_by(gender, education, age_bin) %>% 
      summarise(prop = sum(LDP) / sum(n)) %>% 
      ungroup() %>% 
      pull(prop),
      pch = 16,
      xlim = c(0, 1), 
      ylim = c(0, 1),
      xlab = "Fitted (Pr voting for LDP)", 
      ylab = "Data (Pr voting for LDP)"
)
abline(0, 1)
