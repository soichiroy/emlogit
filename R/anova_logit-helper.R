
##
## Construct constraints for 
## 
## Quadratic programming problem
## 
##    
##    min (1/2) * b'Db - b'd
## 
##    st   lb <= A'b <= ub
## 


#' Create constraint inputs for no-regularization model.
#' 
#' @param pvec A vector of variable levels. 
#' @keywords internal
create_const_unregularized <- function(p_vec) {
  ## 
  ## Example: -------------------------------------------------------
  ##  - p_vec = c(1, 2, 3)
  ##  - first element is always 1 corresponding to the intercept
  ##  - A has the following structure:
  ##
  ##        | 1, 0, 0, 0, 0, 0 |
  ##    A = | 0, 1, 1, 0, 0, 0 |
  ##        | 0, 0, 0, 1, 1, 1 |
  ##
  ## - Then, we set A[1,1] = 0
  ##   because intercept does not have a constraint.
  ## ----------------------------------------------------------------

  ## impose sum to zero constraints
  Amat <- create_Amat_unreg(pvec = p_vec);

  ## intercept is constraint free
  Amat[1,1] <- 0

  # linear equality constraint (sum to zero)
  lb <- ub <- rep(0, length(p_vec))

  return(list(Amat = Amat, lb = lb, ub = ub))
}

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


#' Create constraint inputs for regularization model.
#' @keywords internal
create_const_regularized <- function(p_vec, lambda, wj) {
  ##
  ## Example --------------------------------------------------------
  ## - pvec = c(1, 2, 3)
  ## - A has the following structure
  ##
  ##      | 0 , 0  , 0  | /* no constraints on the intercept
  ##      | 0 , L2 , 0  | /* define pairwise difference
  ##  A = | 0 , 0  , L3 | /* define pairwise difference
  ##      | 0 , w2 , w3 | /* weighted sum of theta+ and theta-
  ##      | 0 , M2 , 0  | /* positivity constarints
  ##      | 0 , 0  , M3 | /* positivity constraints
  ##
  ## where
  ##  Lj = | Dj, I, I |
  ##       |  1, 0, 0 |
  ## with
  ##                            | 1, -1 ,  0 |
  ##  D2 = | 1, -1 |  and  D3 = | 1,  0 , -1 |
  ##                            | 0,  1 , -1 |
  ## and
  ##  Mj = |0, I, 0|
  ##       |0, 0, I|
  ##
  ## ----------------------------------------------------------------

  ## create Amat
  Lj <- Sj <- Pj <- vector("list", length(p_vec))
  Lj[[1]] <- Sj[[1]] <- Pj[[1]] <- Matrix::Matrix(0, nrow = 1, ncol = 1)

  for (j in 2:length(p_vec)) {
    ## creat Dj -- differencing matrix to create a pairwise parameters 
    dj <- p_vec[j] * (p_vec[j] - 1) / 2
    Dj <- Mj <- Matrix::Matrix(0, nrow = dj, ncol = p_vec[j])
    iter <- 1
    for (p1 in 1:(p_vec[j]-1)) {
      for (p2 in (p1+1):p_vec[j]) {
        Dj[iter, p1] <- 1
        Dj[iter, p2] <- -1
        iter <- iter + 1
      }
    }

    ## this is for sum-to-zero constraints
    one_zero_j <- Matrix::Matrix(c(rep(1, p_vec[j]), rep(0, 2 * dj)), nrow = 1)
    Ij <- Matrix::Diagonal(dj)

    Lj[[j]] <- rbind(
      cbind(Dj, Ij, -Ij),
      one_zero_j
    )

    ## create theta+ + theta- <= t constraints
    if (is.null(wj)) {
      weights <- rep(1, 2 * dj)
    } else {
      weights <- rep(wj[[j]], 2)
    }

    zero_ones <- Matrix::Matrix(c(rep(0, p_vec[j]),
                                  weights), nrow = 1)
    Sj[[j]] <- zero_ones

    ## positivity constraints
    zeros <- Matrix::Matrix(0, nrow = dj, ncol = dj)
    Pj[[j]] <- rbind(cbind(Mj, Ij, zeros), cbind(Mj, zeros, Ij))

  }

  ## create a block diagonal of Lj's
  Lj_block <- Matrix::bdiag(Lj)
  
  ## concatinate Sj = (w2, w3, ...)
  Sj_block <- do.call(cbind, Sj)
  
  ## create a block diagonal of Mj's 
  Pj_block <- Matrix::bdiag(Pj)[-1,]
  
  ## combine all 
  Amat     <- rbind(Lj_block, Sj_block, Pj_block)


  ##
  ## Define: lb <= Ax <= ub
  ##
  epsilon <- 1e-8
  ub <- rep(epsilon, nrow(Lj_block))    #* Lj * b = 0
  lb <- rep(-epsilon, nrow(Lj_block))   #* Lj * b = 0
  lb <- c(lb, 0)                        #* lb for Sj = 0
  ub <- c(ub, lambda)                   #* ub for Sj = lambda
  lb <- c(lb, rep(0, nrow(Pj_block)))   #* positivity constrains on theta+-
  ub <- c(ub, rep(Inf, nrow(Pj_block))) #* no upper bound
  
  return(list(Amat = Amat, lb = lb, ub = ub))
}





#' Subset beta
#' @keywords internal
al_subset_beta <- function(beta_full, p_vec, regularize) {
  ## beta_full is a block of vectors 
  ## beta = (beta1, beta2, beta3, ...) where 
  ## beta1 = (coefs_1, theta+_1, theta-_1)
  ##
  ## coefs are used to comptue X'b 
  ## theta+ and theta- are slack variables defined such that 
  ## coefs_1[k] - coefs_1[l] = theta+ - theta- with proper indexing 
  ## 
  if (isFALSE(regularize)) return(beta_full)
  
  beta_sub <- vector("list", length = length(p_vec))
  beta_sub[[1]] <- beta_full[1]
  csum <- cumsum(p_vec)
  lb <- csum[1] + 1
  for (j in 2:length(p_vec)) {
    dj <- p_vec[j] * (p_vec[j] - 1) / 2
    beta_sub[[j]] <- beta_full[lb:(lb + p_vec[j]-1)]
    lb <- lb + p_vec[j] + 2 * dj
  }

  return(unlist(beta_sub))
}
