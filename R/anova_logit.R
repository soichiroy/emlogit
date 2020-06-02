


##
## ANOVA logit
##

#' Logistic regression with ANOVA style parametrization.
#' @param y a vector of binary responses.
#' @param X a matrix of overparametrized
anova_logit <- function(y, X, option = list()) {

  ## set the default value of the option parameters
  option <- al_set_option(option)

  ## EM
  params <- al_em_run(y, X, option)


  ## fitted value
  betas  <- params[[1]][['beta']]
  fitted <- 1 / (1 + exp(-as.vector(X %*% betas)))

  ## return estimates
  option$iteration   <- params$iteration
  option$convergence <- params$convergence
  attr(betas, "option") <- option
  attr(betas, "fitted") <- fitted
  class(betas) <- c("anova_logit")
  return(betas)
}


#' E-step function to compute the expectation of omega.
#' @keywords internal
al_estep <- function(X, betas) {
  Xb <- X %*% betas
  omega <- as.vector(tanh(Xb / 2) / (2 * Xb))
  return(omega)
}

#' M-step function to solve quadratic programming.
#' @keywords internal
al_mstep <- function(y, X, omega, p_vec) {


  ## prepare inputs
  Dmat <- t(X) %*% diag(omega) %*% X
  # Dmat <- Matrix::nearPD(t(X) %*% diag(omega) %*% X)$mat
  dvec <- as.vector(t(X) %*% diag(omega) %*% ((y - 1/2) / omega))  ## change this to accomodate binomial cases n[i] > 1
  Amat <- create_Amat(pvec = p_vec); Amat[1,1] <- 0
  bvec <- rep(0, length(p_vec))

  ## solve QP
  ## can we pass the initial base?
  # fit <- quadprog::solve.QP(Dmat, dvec, Amat)
  settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-8, eps_rel = 1e-8)
  osqp_solution <- osqp::solve_osqp(
      Dmat, -dvec, Amat, l = bvec, u = bvec, pars = settings)

  return(osqp_solution$x)

}


#' Create A matrix for linear constraints
#' @keywords internal
create_Amat <- function(pvec) {

  n_rows <- length(pvec)
  n_cols <- sum(pvec)
  Amat   <- matrix(0, nrow = n_rows, ncol = n_cols)

  lb <- 1; ub <- pvec[1]
  for (i in seq_len(n_rows)) {
    Amat[i,lb:ub] <- 1
    ## update lower bound
    lb <- lb + pvec[i]
    ## update upper bound
    if (i < n_rows) ub <- ub + pvec[i+1]
  }

  return(Amat)

}

#' EM-algorithm for the ANOVA logit
#' @keywords internal
al_em_run <- function(y, X, option) {

  ## initialize parameters
  params <- vector("list", length = option$max_iter)
  params <- al_params_initialize(X, params)

  ##
  ## Run EM algorithm
  ##
  for (iter in seq_len(option$max_iter)) {
    ## E-step ---------------------------------------------
    params[[iter]]$omega <- al_estep(X, params[[iter]]$beta)

    ## M-step ---------------------------------------------
    params[[iter+1]]$beta  <- al_mstep(y, X, params[[iter]]$omega, option$pvec)

    ## check_convergence
    if (iter > 1 &&
        al_check_convergence(params[c(iter - 1, iter)]) < option$tol) {
        break;
    }
  }


  ## remve NULL elements
  par <- params[purrr::map_lgl(params, ~!is.null(.x))]
  par <- par[length(par)]
  par$iteration <- iter
  par$convergence <- if_else(iter < option$max_iter, 1, 0)
  return(par)
}



al_set_option <- function(option) {
  ## set maximum number of iterations
  if (isFALSE(exists("max_iter", option))) option$max_iter <- 200

  ## set tolerance parameter for the convergence
  if (isFALSE(exists('tol', option))) option$tol <- 1e-5

  ##

  return(option)
}



al_check_convergence <- function(params) {
  max_diff <- max(abs(unlist(params[[1]]) - unlist(params[[2]]))) / abs(unlist(params[[1]]))
  return(max_diff)

}


al_params_initialize <- function(X, params) {
  params[[1]]$beta <- rnorm(ncol(X), 0, 0.5)
  return(params)
}



#' Compute predicted probabilities
#' @export
predict.anova_logit <- function(fit, newdata = NULL) {
  if (is.null(newdata)) {
    fitted <- attr(fit, "fitted")
  } else {
    fitted <- 1 / (1 + exp(-newdata %*% fit))
  }
  return(fitted)
}
