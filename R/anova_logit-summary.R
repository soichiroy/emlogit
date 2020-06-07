
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
