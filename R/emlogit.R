


#' EM algorithm for Multinomial Logit
#'
#' @param Y A matrix of multinomial outcome where rows correspond to observations
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

  # max_iter = 200, tol = 1e-5, verbose = FALSE, intercept = TRUE)
  if (!exists("max_iter", control)) {
    control$max_iter <- 200
  }

  if (!exists("tol", control)) {
    control$tol <- 1e-6
  }

  if (!exists("verbose", control)) {
    control$verbose = FALSE
  }


  if (isTRUE(control$intercept)) {
    X <- cbind(rep(1, nrow(X)), X)
  }

  n_cov <- ncol(X)
  J     <- ncol(Y)
  B <- cbind(rep(0, n_cov),
    matrix(rnorm(n_cov * (J-1)), nrow = n_cov, ncol = J-1))

  if (!exists("mu0", control)) {
    control$mu0 <- rep(0, ncol(X))
  }

  if (!exists("Z0", control)) {
    control$Z0 <- diag(rep(5, ncol(X)))
  }

  fit <- emlogit_run(
      Y = Y, X = X, B = B,
      tol = control$tol, max_iter = control$max_iter,
      mu0 = control$mu0, Z0 = control$Z0, verbose = control$verbose
    )

  ## col
  fit <- tibble::as_tibble(fit)
  colnames(fit) <- paste("J = ", 1:ncol(Y), sep = "")
  # if (!is.null(colnames(X))) {
  #   rownames(fit) <- c("(Intercept)", colnames(X))
  # } else {
  #   rownames(fit) <- paste("coef", 1:(ncol(X)))
  # }
  #

  class(fit) <- c(class(fit), "emlogit", "emlogit.est")
  return(fit)
}
