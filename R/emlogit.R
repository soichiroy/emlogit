


#' EM algorithm for Multinomial Logit
#'
#' @param Y A matrix of multinomial outcomes where rows correspond to observations
#'  and columns correspond to choices.
#' @param X A matrix of covariates where columns correspond to variables.
#'  The number of rows should match with the number of rows for \code{Y}.
#' @param control A list of control parameters, which can contain the following items:
#' \itemize{
#'   \item \code{max_iter} A integer value that specifies the maximum iterations
#'   for the EM algorithm. Defaul value is 200.
#'   \item \code{tol} A tolerance parameter for assessing the convergence. Default value is 1e-5.
#'   \item \code{mu0} A vector of prior means. The dimension of this parameter should match with the dimension of \code{X} (i.e., the nmber of variables).
#'    The default value is \code{0}.
#'   \item \code{Z0} A matrix for the prior variance covariance matrix.
#'      The dimension of this matrix should match with the dimension of \code{X} i.e., the nmber of variables).
#'      The default vaue is \code{diag(rep(5, ncol(X)))}
#'   \item \code{verbose} A boolean argument. If set \code{TRUE}, the function shows the log-posterior for each iteration. Default is \code{FALSE}.
#'   \item \code{intercept} A boolean argument. When \code{X} already contains the intercept term (i.e., a column of ones), this option should \code{FALSE}.
#'      Default is \code{TRUE}.
#' }
#' @export
emlogit <- function(Y, X, control = list()) {

  ## setup ------------------------------------------------------------
  X <- as.matrix(X)
  control <- input_check(control)
  if (isTRUE(control$intercept)) {
    X <- cbind(rep(1, nrow(X)), X)
  }

  control <- specify_prior(control, ncol(X))
  B <- coef_initialize(Y, X)

  ## estimate coefficeints using EM -----------------------------------
  coef <- emlogit_run(
      Y = Y, X = X, B = B,
      tol = control$tol, max_iter = control$max_iter,
      mu0 = control$mu0, Z0 = control$Z0, verbose = control$verbose
    )

  ## compute variance of coefficeints ---------------------------------
  var <- emlogit_var(Y, X, B, control$mu0, control$Z0)

  ## obtain the in-sample fit -----------------------------------------
  prob <- predict_prob(X, coef)
  fit <- list(coef = coef, var = var, prob = prob, control = control)

  class(fit) <- c("emlogit", "emlogit.est")
  return(fit)
}


#' Input check
#' A function to set the default values of \code{control}.
#' @keywords internal
input_check <- function(control) {
  if (!exists("max_iter", control)) {
    control$max_iter <- 200
  }

  if (!exists("tol", control)) {
    control$tol <- 1e-6
  }

  if (!exists("verbose", control)) {
    control$verbose = FALSE
  }

  if (!exists("intercept", control)) {
    control$intercept <- TRUE
  }
  return(control)
}

#' Specify the prior
#' @keywords internal
specify_prior <- function(control, n_cov) {
  if (!exists("mu0", control)) {
    control$mu0 <- rep(0, n_cov)
  }

  if (!exists("Z0", control)) {
    control$Z0 <- diag(rep(5, n_cov))
  }
  return(control)
}

#' Initialize coefficeints
#' @keywords internal
coef_initialize <- function(Y, X) {
  n_cov <- ncol(X)
  J     <- ncol(Y)
  B     <- cbind(rep(0, n_cov),
                 matrix(rnorm(n_cov * (J-1)), nrow = n_cov, ncol = J-1))
  B <- as.matrix(B)
  return(B)
}
