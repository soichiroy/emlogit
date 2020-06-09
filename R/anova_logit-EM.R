##
## Functions for implementing EM algorithm for ANOVA Logit 
## 
## * al_em_run() is the main function that calls: 
##  - al_estep() for E-step. 
##  - al_mstep() for M-step. 
## 
## * Helper functions: 
##  - al_log_likelihood() for computing log-likelihood.
##  - al_compute_BIC() for computing BIC used to select regularization parameters.
##



#' EM-algorithm for the ANOVA logit
#' @keywords internal
al_em_run <- function(y, X, trials, option) {

  ## initialize parameters
  params <- vector("list", length = option$max_iter + 1)
  params <- al_params_initialize(X, params)

  ## define constrains
  opt_input <- al_set_opt_input(option$pvec, option$regularize, option$lambda, option$wj)

  ## Expand the design matrix
  Xe <- al_expand_X(X, option$pvec, option$regularize)

  # set trials
  if (is.null(trials)) trials <- rep(1, length(y))

  ##
  ## Run EM algorithm
  ##
  for (iter in seq_len(option$max_iter)) {
    ## E-step ---------------------------------------------
    params[[iter]]$omega <- al_estep(X, trials, params[[iter]]$beta)

    ## M-step ---------------------------------------------
    beta_full <- al_mstep(y, Xe, trials, params[[iter]]$omega, opt_input)

    ## discard slack variables ----------------------------
    params[[iter+1]]$beta <- al_subset_beta(
      beta_full, option$pvec, option$regularize
    )

    ## check_convergence ----------------------------------
    if (iter > 1 && 
        al_check_convergence(params[c(iter - 1, iter)]) < option$tol) break;

  } ## end of estimation iterations

  ## remve NULL elements and returns the final estimate
  params          <- params[purrr::map_lgl(params, ~!is.null(.x))]
  par             <- vector("list")
  par$coefficient <- params[[length(params)]][['beta']]
  par$iteration   <- iter
  par$convergence <- if_else(iter < option$max_iter, 1, 0)

  ## comptue BIC
  par$BIC <- al_compute_BIC(y, X, trials, par$coefficient, option$pvec)

  return(par)
}


##
## EM functions
##

#' E-step function to compute the expectation of omega.
#' @keywords internal
al_estep <- function(X, trials, betas) {
  Xb    <- X %*% betas
  omega <- as.vector(tanh(Xb / 2) * trials / (2 * Xb))
  return(omega)
}

#' M-step function to solve quadratic programming.
#' @import Matrix
#' @keywords internal
al_mstep <- function(y, X, trials, omega, opt_input) {
  ## prepare inputs
  XO   <- t(X) %*% diag(omega)
  Dmat <- XO %*% X
  dvec <- as.vector(XO %*% ((y - trials / 2) / omega))

  ## constraints
  Amat   <- opt_input$Amat
  lb_vec <- opt_input$lb
  ub_vec <- opt_input$ub

  ## solve QP
  settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-5, eps_rel = 1e-5)
  fit      <- osqp::solve_osqp(Dmat, -dvec, Amat, l = lb_vec, u = ub_vec, pars = settings)

  return(fit$x)
}


#' Check convergence by computing the maximum rate of change
#' @keywords internal
al_check_convergence <- function(params) {
  ## compute max( |beta[t] - beta[t-1]| / |beta[t-1]| )
  max_diff <- max( abs(unlist(params[[1]]) - unlist(params[[2]])) /
                      abs(unlist(params[[1]])) )
  return(max_diff)
}



#' Likelihood function
#' @keywords internal
al_log_likelihood <- function(y, X, trials, betas) {
  if (is.null(trials)) trials <- rep(1, length(y))

  ## Compute Pr(Y = 1 | X)
  pY <- 1 / (1 + exp(- X %*% betas))

  ## log-likelihood
  ll <- sum(y * log(pY) + (trials - y) * log(1 - pY))

  return(ll)

}


#' Compute BIC
#' @keywords internal
al_compute_BIC <- function(y, X, trials, betas, p_vec, thres = 1e-4) {
  betas[abs(betas) <= thres] <- 0
  np   <- length(p_vec)
  nobs <- length(y)

  ## compute the degree of freedom
  df <- sum(betas != 0) - (np - 1)

  ## BIC = df * log(n) - 2 * LL
  BIC <- log(nobs) * df - 2 * al_log_likelihood(y, X, trials, betas)
  return(BIC)
}
