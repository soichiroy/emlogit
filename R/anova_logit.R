


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

  ## return estimates
  attr(params, "option") <- option
  class(params) <- c("anova_logit")
  return(params)
}


#' E-step function to compute the expectation of omega.
#' @keywords internal
al_estep <- function(X, betas) {
  Xb <- X %*% betas
  omega <- tanh(Xb / 2) / (2 * Xb)
  return(omega)
}

#' M-step function to solve quadratic programming.
#' @keywords internal
al_mstep <- function() {

}


#' EM-algorithm for the ANOVA logit
#' @keywords internal
al_em_run <- function(y, X, option) {

  ## initialize parameters
  params <- vector("list", length = option$max_iter)
  params <- al_params_initialize(params)

  ##
  ## Run EM algorithm
  ##
  for (iter in seq(2, option$max_iter, 1)) {
    ## E-step ---------------------------------------------
    params[[iter]]$omega <- al_estep(X, params[[iter]]$beta)

    ## M-step ---------------------------------------------
    params[[iter+1]]$beta  <- al_mstep(X, params[[iter]]$omega)

    ## check_convergence
    if (iter > 1 &&
        al_check_convergence(params[c(iter - 1, iter)]) < tol) {
        break;
    }
  }


  ## remve NULL elements
  par <- params[purrr::map_lgl(params, ~!is.null(.x))]
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
