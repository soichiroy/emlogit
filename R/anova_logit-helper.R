#' Create A matrix for linear constraints
#' @keywords internal
create_Amat <- function(pvec) {

  n_rows <- length(pvec)
  n_cols <- sum(pvec)
  Amat   <- matrix(0, nrow = n_rows, ncol = n_cols)

  lb <- 1; ub <- pvec[1]
  for (i in seq_len(n_rows)) {
    Amat[i,lb:ub] <- 1
    ## update lower bound
    lb <- lb + pvec[i]
    ## update upper bound
    if (i < n_rows) ub <- ub + pvec[i+1]
  }

  return(Amat)

}
