
#' Print function
#' @param obj An output of \code{summary.emlogit()} function.
#' @import tibble
#' @importFrom utils getFromNamespace
#' @export
print.summary.emlogit <- function(obj) {
  # print(obj)
  print_tbl <- getFromNamespace("print.tbl", "tibble")
  print_tbl(obj)
  invisible(obj)
}

#' Coef function
#' @inheritParams summary.emlogit
#'
coef.emlogit <- function(obj) {
  # Names of the output levels
  if (is.null(obj$y_name))
    colnames(obj$coef) <- paste("`", 1:ncol(obj$coef), "`", sep = "")
  else
    colnames(obj$coef) <- obj$y_name

  # Names of the covariates
  if (is.null(obj$x_name)) {
    rownames(obj$coef) <- 1:nrow(obj$coef)
  } else {
    if (isTRUE(obj$control$intercept)) obj$x_name[1] <- "intercept"
    rownames(obj$coef) <- obj$x_name
  }

  obj$coef
}

#' Summary function
#' @param obj An object class of \code{"emlogit.est"}
#' @importFrom dplyr %>% mutate select arrange left_join
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer everything
#' @export
summary.emlogit <- function(obj) {
  if ("emlogit.est" %in% class(obj)) {

    ## create a matrix of se
    se_mat <- sqrt(obj$var)

    ## create a long coef
    if (is.null(obj$y_name)) {
      colnames(se_mat)   <- paste("`", 2:ncol(obj$coef), "`", sep = "")
    } else {
      colnames(se_mat)   <- obj$y_name[-1]
    }

    coef_name <- rownames(coef(obj))

    coef_long <- tibble::as_tibble(coef(obj)[,-1, drop=FALSE]) %>%
      mutate(betas = coef_name) %>%
      tidyr::pivot_longer(-betas, names_to = "category", values_to = "estimate")

    ## create a long form se
    var_long <- tibble::as_tibble(se_mat) %>%
      mutate(betas = coef_name) %>%
      tidyr::pivot_longer(-betas, names_to = "category", values_to = "se")
    tab <- coef_long %>%
      left_join(var_long, by = c("category", "betas")) %>%
      select(category, everything()) %>%
      arrange(category)
  } else {
    stop("Not a supported input.")
  }


  class(tab) <- c("summary.emlogit", class(tab))
  return(tab)
}




#' Obtain the predicted probability
#' @param obj An output of \code{emlogit()} funcion.
#' @param newdata A matrix of newdata. If not provided, the insample fit is returned.
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

  if (!is.null(obj$y_name)) {
    colnames(pred) <- obj$y_name
  }
  return(pred)
}
