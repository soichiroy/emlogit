##
## Example
##

require(dplyr)
require(purrr)
require(stringr)

## Example data 
data(japan, package = "MNP")
japan <- japan %>%
  mutate(n = LDP + NFP + SKG + JCP) %>%
  mutate(age_bin = ntile(age, 10))


## 
## Example without regularization
## 

fit <- anova_logit(
  formula = LDP | n ~  gender + age_bin + education,
  data = japan,
  option = list(regularize = FALSE)
)

## view coefficients
round(fit$coef, 3)

## fitted values 
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


## 
## Example with regularization
## 
set.seed(1235)
fit <- anova_logit(
  formula = LDP | n ~  gender + age_bin + education,
  data    = japan,
  option  = list(regularize = TRUE, lambda = 3, adaptive_weight = FALSE)
)

round(fit$coef, 3)



## 
## Example with regularization -- regularization path
## 

## additional packages 
require(future)
require(furrr)
require(ggplot2)
require(purrr)
require(dplyr)

## range of regularization parameters
lambda_vec <- exp(seq(-3.1, 3, by = 0.1))

## setup parallel computation
plan(multiprocess)

## compute coef for each lambda 
set.seed(1234)
fit_reg <- furrr::future_map(
  lambda_vec, ~ anova_logit(
    LDP | n ~  gender + age_bin + education,
    data = japan, 
    option = list(regularize = TRUE, lambda = .x, adaptive_weight = FALSE)
  ), .progress = TRUE
)


## solution path 

## solution path
purrr::map(fit_reg, ~.x$coef) %>%
  do.call(rbind, .) %>%
  matplot(., type = 'l')

coef_path <- purrr::map_dfr(fit_reg, ~ enframe(.x$coef, 
    name = 'variable', value = 'coef'), .id = 'lambda') %>% 
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
  

## plot BIC 
purrr::map_dbl(fit_reg, ~.x$BIC) %>%
  rlang::set_names(lambda_vec) %>%
  enframe() %>%
  ggplot(aes(x = as.numeric(name), y = value)) +
  geom_line() +
  geom_point() +
  theme_bw()
