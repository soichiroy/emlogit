#' Create A matrix for linear constraints
#' @keywords internal
create_Amat_unreg <- function(pvec) {

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




#' Subset beta
#' @keywords internal
al_subset_beta <- function(beta_full, p_vec) {

  beta_sub <- vector("list", length = length(p_vec))
  beta_sub[[1]] <- beta_full[1]
  lb <- 2; ub <- 0
  for (j in 2:length(p_vec)) {
    dj <- p_vec[j] * (p_vec[j] - 1) / 2
    ub <- ub + (p_vec[j] + 2 * dj)
    beta_sub[[j]] <- beta_full[lb:(lb + p_vec[j]-1)]
    lb <- lb + ub
  }
  return(unlist(beta_sub))
}
