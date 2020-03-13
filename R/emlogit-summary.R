
#' Print function
#' @import tibble
#' @export
print.summary.emlogit <- function(obj) {
  # print(obj)
  tibble:::print.tbl(obj)
  invisible(obj)
}


#' Summary function
#' @param obj An object class of \code{"emlogit.est"}
#' @importFrom dplyr %>% mutate select arrange left_join
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @export
summary.emlogit <- function(obj) {
  if ("emlogit.est" %in% class(obj)) {

    ## create a long coef
    colnames(obj$coef) <- paste("`", 1:ncol(obj$coef), "`", sep = "")
    coef_long <- tibble::as_tibble(obj$coef[,-1]) %>%
      mutate(betas = 1:nrow(obj$coef)) %>%
      tidyr::pivot_longer(-betas, names_to = "category", values_to = "estimate")

    ## create a long form se
    se_mat <- sqrt(obj$var)
    colnames(se_mat) <- paste("`", 2:ncol(obj$coef), "`", sep = "")
    var_long <- tibble::as_tibble(se_mat) %>%
      mutate(betas = 1:nrow(se_mat)) %>%
      tidyr::pivot_longer(-betas, names_to = "category", values_to = "se")
    tab <- coef_long %>%
      left_join(var_long, by = c("category", "betas")) %>%
      select(category, everything()) %>%
      arrange(category, betas)
  } else {
    stop("Not a supported input.")
  }


  class(tab) <- c("summary.emlogit", class(tab))
  return(tab)
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
