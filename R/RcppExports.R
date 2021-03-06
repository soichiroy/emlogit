# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Predicted probability
#' @param X A matrix of covariates.
#' @param B A matrix of coefficeints including the reference category.
#' @keywords internal
predict_prob <- function(X, B) {
    .Call('_emlogit_predict_prob', PACKAGE = 'emlogit', X, B)
}

#' observed log-likelihood
#' @param Y A matrix of multinomial outcomes.
#' @param X A matrix of covariate matrix.
#' @param B A matrix of coefficeints. Cols correspond to choices and rows correspond to variables.
#' @param mu0 A vector of prior means.
#' @param Z0 A matrix of prior variance covariance matrix.
#' @return A double of log-likelihood evaluated at \code{B}.
#' @keywords internal
log_likelihood <- function(Y, X, B, mu0, Z0) {
    .Call('_emlogit_log_likelihood', PACKAGE = 'emlogit', Y, X, B, mu0, Z0)
}

#' EM implementation
#' @keywords internal
emlogit_run <- function(Y, X, B, tol, max_iter, mu0, Z0, verbose) {
    .Call('_emlogit_emlogit_run', PACKAGE = 'emlogit', Y, X, B, tol, max_iter, mu0, Z0, verbose)
}

#' Variance computation
#' @keywords internal
emlogit_var <- function(Y, X, B, mu0, Z0, robust) {
    .Call('_emlogit_emlogit_var', PACKAGE = 'emlogit', Y, X, B, mu0, Z0, robust)
}

