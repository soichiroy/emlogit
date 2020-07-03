

## load data 
require(tidyverse)
require(haven)
require(labelled)
require(mTensor)
data(acs2017TX_educ)
data(acs2017TX_race)
data(cces2018TX)


cces2018TX <- cces2018TX %>% 
  filter(citizen == 1)

## 
## compute CD level targets 
## 
compute_prop <- function(dat, var_name) {
  summary_dat <- dat %>% 
    group_by_at(vars(one_of(var_name))) %>% 
    summarise(n = sum(count)) %>% 
    ungroup() %>% 
    mutate(prop = n / sum(n))  %>% 
    select(-n) 
  return(summary_dat)
}

## main terms 
pop_cd     <- compute_prop(acs2017TX_educ, c("cd"))
pop_educ   <- compute_prop(acs2017TX_educ, c("educ"))
pop_age    <- compute_prop(acs2017TX_educ, c("age"))
pop_gender <- compute_prop(acs2017TX_educ, c("gender"))
pop_race   <- compute_prop(acs2017TX_race, c("race"))

## interaction by cd 
pop_cd_educ   <- compute_prop(acs2017TX_educ, c("cd", "educ"))
pop_cd_age    <- compute_prop(acs2017TX_educ, c("cd", "age"))
pop_cd_gender <- compute_prop(acs2017TX_educ, c("cd", "gender"))
pop_cd_race   <- compute_prop(acs2017TX_race, c("cd", "race"))


##
## create survey matrix 
## 
cd_mat <- model.matrix(~ cd - 1, data = cces2018TX)
  
gender_mat <- cces2018TX %>% 
  mutate(gender = to_factor(gender)) %>% 
  model.matrix(~ gender - 1, data = .)
# gender_mat <- gender_mat[, 'genderMale', drop = FALSE]

race_mat <- cces2018TX %>% 
  mutate(race = to_factor(race)) %>% 
  model.matrix(~ race - 1, data = .)
# race_mat[,"raceAll Other"] <- race_mat[,"raceAll Other"] + race_mat[,"raceAsian"]
# race_mat <- race_mat[, c("raceWhite", "raceBlack", "raceHispanic")]

age_mat <- cces2018TX %>% 
  mutate(age = to_factor(age)) %>% 
  model.matrix(~ age - 1, data = .)
# age_mat <- age_mat[,-ncol(age_mat)]
educ_mat <- cces2018TX %>% 
  mutate(educ = to_factor(educ)) %>% 
  model.matrix(~ educ - 1, data = .)
# educ_mat <- educ_mat[,-ncol(educ_mat)]



cd_gender <- cces2018TX %>% 
  mutate(gender = to_factor(gender)) %>% 
  model.matrix(~ cd:gender - 1, data = .)
cd_race <- cces2018TX %>% 
  mutate(race = to_factor(race)) %>% 
  model.matrix(~ cd:race - 1, data = .)
cd_age <- cces2018TX %>% 
  mutate(age = to_factor(age)) %>% 
  model.matrix(~ cd:age - 1, data = .)
cd_educ <- cces2018TX %>% 
  mutate(educ = to_factor(educ)) %>% 
  model.matrix(~ cd:educ - 1, data = .)

apply(cd_race, 2, mean)


pop_vector <- c(
  pop_cd$prop, 
  pop_educ$prop, 
  pop_cd_educ$prop
)

X <- cbind(
  cd_mat, 
  educ_mat, 
  cd_educ
)

par_init <- rnorm(ncol(X)+1)

require(Matrix)
require(purrr)
level_main <- list(1, ncol(cd_mat), ncol(educ_mat))
level_inter <- list(list(ncol(cd_educ), c(2, 3)))
constMat    <- cbsw_inter_input(level_main, level_inter)
Mmat        <- Reduce("+", purrr::map(constMat, ~t(.x) %*% .x))
uvec        <- map(constMat, ~runif(nrow(.x)));
phi         <- cbsw_inter_update_phi(par_init, constMat, uvec)


cbsw_inter_loss(
  par = par_init, 
  Xmat = cbind(rep(1, nrow(X)), X), 
  x_pop = c(1, pop_vector), 
  Mmat, constMat, uvec, phi, rho = 1)

cbsw_inter_loss_cpp(
  par = par_init, 
  Xmat = cbind(rep(1, nrow(X)), X), 
  pop_vec = c(1, pop_vector), 
  Mmat, constMat, uvec, phi, rho = 1)


gr <- cbsw_inter_loss_gradient(
    par = par_init, 
    Xmat = cbind(rep(1, nrow(X)), X), 
    x_pop = c(1, pop_vector), 
    Mmat, constMat, uvec, phi, rho = 1
)

gr_cpp <- cbsw_inter_loss_gradient_cpp(
    par = par_init, 
    Xmat = cbind(rep(1, nrow(X)), X), 
    pop_vec = c(1, pop_vector), 
    Mmat, constMat, uvec, phi, rho = 1
)


fit_test <- optim(
  par = runif(length(par_init)), 
  fn  = cbsw_inter_loss_cpp,
  gr  = cbsw_inter_loss_gradient_cpp,
  Xmat = cbind(rep(1, nrow(X)), X), 
  pop_vec = c(1, pop_vector), 
  Mmat  = Mmat, 
  constMat = constMat, 
  uvec = uvec, phi = phi, rho = 1,
  method = "BFGS"
)

require(lbfgs)
require(future)
require(future.apply)

plan(multiprocess)

norm2 <- function(x) sqrt(sum(x^2))



eta <- rnorm(length(par_init))
loss_trace <- pr_resid <- du_resid <- rep(NA, 20)
for (i in 1:50) {
  cat("currently at ", i, "\n")
  phi_old <- phi 
  fit <- lbfgs(
    cbsw_inter_loss_cpp, cbsw_inter_loss_gradient_cpp,
    vars = eta,
    Xmat = cbind(rep(1, nrow(X)), X), 
    pop_vec = c(1, pop_vector), 
    Mmat  = Mmat, 
    constMat = constMat, 
    uvec = uvec, phi = phi, rho = 1,
    invisible = 1
  )
  
  eta <- fit$par 
  loss_trace[i] <- fit$value

  phi <- cbsw_inter_update_phi(
    eta, constMat, uvec, w = 0.2
  )

  uvec <- cbsw_inter_update_u(eta, phi, uvec, constMat) 
  
  ## comptue primal residual 
  pr_resid[i] <- sum(future_sapply(1:length(constMat), function(i) {
    norm2(constMat[[i]] %*% eta - phi[[i]])
  }))
  
  ## compute dual residual
  du_resid[i] <- sum(future_sapply(1:length(constMat), function(i) {
    norm2(t(constMat[[i]]) %*% (phi[[i]] - phi_old[[i]]))
  }))
}


par(mfrow = c(1, 2))
plot(pr_resid, type = 'l')
plot(du_resid, type = 'l')



ebal_fit <- ebal_survey(X, pop_vector, add_intercept = TRUE)

enframe(apply(X, 2, weighted.mean, w = ebal_fit$w))
