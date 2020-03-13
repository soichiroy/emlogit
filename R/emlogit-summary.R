


#' Summary function
#' @param obj An object class of \code{"emlogit.est"}
#' @export
summary.emlogit.est <- function(obj) {
  if ("emlogit.est" %in% class(obj)) {
    tab <- obj[,-1]
  } else {
    stop("Not a supported input.")
  }
  return(tab)
}

#' @export
print.summary.emlogit.est <- function(obj) {
  print(obj)
}
