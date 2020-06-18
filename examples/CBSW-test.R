

require(synthArea)
require(dplyr)
require(labelled)
require(haven)
data(cces_survey)



nc_sample <- cces_survey %>%
  filter(st == "NC") %>%
  na.omit()


xx <- emlogit::al_data(
  vv_turnout ~  pid3 + newsint,
  data = nc_sample %>%
    mutate(pid3 = as_factor(pid3), 
          newsint = as_factor(newsint))
)

# debugonce(create_input_matrix)
ct <- create_input_matrix(as.list(xx$pvec))
pm <- cbsw_prepare_params(ct)


ct$lambda  <- 1
ct$rho     <- 1/2
fit <- cbsw_admm(pm, xx, ct, option = list(max_iter = 50, tol = 1e-5,
  use_grad = FALSE))


y <- ct$B %*% pm$eta + pm$w
Ab <- ct$A %*% pm$beta
Xb <- xx$X %*% pm$beta
S  <- xx$y
obj_main <- sum(-S * exp(-Xb) - (1 - S) * Xb)
obj_pen  <- as.vector(ct$rho/2 * (t(Ab) %*% Ab + 2 * t(y) %*% Ab))

debugonce(cbsw_main_objective_fn)
cbsw_main_objective_fn(
  par = pm$beta, S = xx$y, X = xx$X, params = pm,
  A = ct$A, B = ct$B, rho = ct$rho)


ct$A %*% pm$beta +
ct$B %*% pm$eta +
pm$w
