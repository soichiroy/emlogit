


##
## ANOVA logit
##

#' Logistic regression with ANOVA style parametrization.
#' @param y A vector of binary responses. In the binomial response, this corresponds to "success".
#' @param X A matrix of overparametrized
#' @param trials A vector of trial counts. In the binary outcome this is left as \code{NULL}.
#'  The lengh of this vector should match the length of \code{y}.
#' @param option A list of options. \code{pvec} should be provided.
#' @export
anova_logit <- function(y, X, trials = NULL, option = list()) {

  ## -------------------------------------------
  ## set the default value of the option parameters
  option <- al_set_option(option)

  ## EM ----------------------------------------
  params <- al_em_run(y, X, trials, option)

  ## fitted value ------------------------------
  betas  <- params$coefficient
  params$fitted <- 1 / (1 + exp(-as.vector(X %*% betas)))

  ## return estimates --------------------------
  attr(params, "option") <- option
  class(params)          <- c("anova_logit")
  return(params)
}


#' EM-algorithm for the ANOVA logit
#' @keywords internal
al_em_run <- function(y, X, trials, option) {

  ## initialize parameters
  params <- vector("list", length = option$max_iter)
  params <- al_params_initialize(X, params)

  ## define constrains
  opt_input <- al_set_opt_input(option$pvec, option$regularize)

  # set trials
  if (is.null(trials)) trials <- rep(1, length(y))

  ##
  ## Run EM algorithm
  ##
  for (iter in seq_len(option$max_iter)) {
    ## E-step ---------------------------------------------
    params[[iter]]$omega <- al_estep(X, params[[iter]]$beta)

    ## M-step ---------------------------------------------
    params[[iter+1]]$beta <- al_mstep(y, X, trials, params[[iter]]$omega, opt_input)

    ## check_convergence
    if (iter > 1 &&
        al_check_convergence(params[c(iter - 1, iter)]) < option$tol
      ) {
        break;
    }
  }


  ## remve NULL elements and returns the final estimate
  params <- params[purrr::map_lgl(params, ~!is.null(.x))]
  par  <- vector("list")
  par$coefficient <- params[[length(params)]][['beta']]
  par$iteration   <- iter
  par$convergence <- if_else(iter < option$max_iter, 1, 0)
  return(par)
}


##
## EM functions
##

#' E-step function to compute the expectation of omega.
#' @keywords internal
al_estep <- function(X, betas) {
  Xb    <- X %*% betas
  omega <- as.vector(tanh(Xb / 2) / (2 * Xb))
  return(omega)
}

#' M-step function to solve quadratic programming.
#' @keywords internal
al_mstep <- function(y, X, trials, omega, opt_input) {
  ## prepare inputs
  XO   <- t(X) %*% diag(omega)
  Dmat <- XO %*% X
  dvec <- as.vector(XO %*% ((y - trials / 2) / omega))

  Amat   <- opt_input$Amat
  lb_vec <- opt_input$lb
  ub_vec <- opt_input$ub

  ## solve QP
  ## can we pass the initial base?
  settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-5, eps_rel = 1e-5)
  fit      <- osqp::solve_osqp(Dmat, -dvec, Amat, l = lb_vec, u = ub_vec, pars = settings)

  return(fit$x)
}


##
## Handling inputs
##

#' Set default opiions
#' @keywords internal
al_set_option <- function(option) {
  ## set maximum number of iterations
  if (isFALSE(exists("max_iter", option))) option$max_iter <- 200

  ## set tolerance parameter for the convergence
  if (isFALSE(exists('tol', option))) option$tol <- 1e-5

  ## set regularization options
  if (isFALSE(exists('regularize', option))) option$regularize <- FALSE

  return(option)
}


#' Define constrains
#' @param pvec A vector of length K where each element corresponds to the level of k-th variable.
#' @param regularize A boolena argument.
al_set_opt_input <- function(p_vec, regularize) {
  if (isTRUE(regularize)) {

  } else {
    ## impose sum to zero constraints
    Amat <- create_Amat(pvec = p_vec);
    ## intercept is constraint free
    Amat[1,1] <- 0
    # linear equality constraint (sum to zero)
    lb <- ub <- rep(0, length(p_vec))
  }

  ## set of constrains
  opt_input <- list(Amat = Amat, lb = lb, ub = ub)

  return(opt_input)
}


#' Check convergence by computing the maximum rate of change
#' @keywords internal
al_check_convergence <- function(params) {
  max_diff <- max(abs(unlist(params[[1]]) - unlist(params[[2]]))) / abs(unlist(params[[1]]))
  return(max_diff)
}

#' Initialize coefficients
#' @keywords internal
al_params_initialize <- function(X, params) {
  params[[1]]$beta <- rnorm(ncol(X), 0, 0.5)
  return(params)
}



#' Compute predicted probabilities
#' @export
predict.anova_logit <- function(fit, newdata = NULL) {
  if (isFALSE('anova_logit' %in% class(fit))) stop("input has to be an 'anova_logit' object.")
  if (is.null(newdata)) {
    fitted <- fit$fitted
  } else {
    fitted <- 1 / (1 + exp(-newdata %*% fit))
  }
  return(fitted)
}
