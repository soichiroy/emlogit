

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


ct$lambda  <- 0.01
ct$rho     <- 1



# debugonce(cbsw_admm)
# debugonce(cbsw_update_beta)
fit <- cbsw_admm(pm, xx, ct, option = list(max_iter = 30, tol = 1e-5,
  use_grad = TRUE))  


weights <- exp(-xx$X %*% fit$beta)

cbind(
colSums(xx$X[xx$y == 1, ] * weights[xx$y == 1]) / sum(xx$y == 0),
colMeans(xx$X[xx$y == 0, ])
)


# debugonce(cvCBSW)
set.seed(1234)
cv_fit <- cvCBSW(
    vv_turnout ~  pid3 + newsint,
    data = nc_sample %>%
      mutate(pid3 = as_factor(pid3), 
             newsint = as_factor(newsint)), 
    option = list()         
)



coef_mat <- map(cv_fit, ~.x$beta) %>% 
  do.call(rbind, .) %>% 
  data.matrix()
  
par(mfrow = c(1, 3))
matplot(coef_mat[,1], type = 'l')  
matplot(coef_mat[,2:6], type = 'l')  
matplot(coef_mat[,7:11], type = 'l')  
