


#' Summary function
#' @param obj An object class of \code{"emlogit.est"}
#' @export
summary.emlogit <- function(obj) {
  if ("emlogit.est" %in% class(obj)) {
    tab <- obj$coef[,-1]
  } else {
    stop("Not a supported input.")
  }
  return(tab)
}

#' @export
print.summary.emlogit <- function(obj) {
  print(obj)
}



#' Obtain the predicted probability
#' @export
predict.emlogit <- function(obj, newdata = NULL) {

  if (is.null(newdata)) {
    pred <- obj$prob
  } else {
    if (isTRUE(obj$control$intercept)) {
      newdata <- cbind(rep(1, nrow(newdata)), newdata)
    }
    pred <- predict_prob(newdata, obj$coef)
  }

  return(pred)
}
