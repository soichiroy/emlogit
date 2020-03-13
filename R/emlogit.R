


#' EM algorithm for Multinomial Logit
#'
#' @param Y A matrix of multinomial outcome where rows correspond to observations
#'  and columns correspond to choices.
#' @param X A matrix of covariates where columns correspond to variables.
#'  The number of rows should match with the number of rows for \code{Y}.
#' @param control A list of the following items:
#' \itemize{
#'   \item \code{max_iter} A integer value that specifies the maximum iterations
#'   for the EM algorithm. Defaul value is 200.
#'   \item \code{tol} A tolerance for assessing the convergence. Default value is 1e-5.
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
