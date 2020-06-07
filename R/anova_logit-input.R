#' Prepare Data
#' @import Formula
#' @import Matrix
#' @importFrom rlang !! sym
#' @export
al_data <- function(formula, data, adaptive_weight = FALSE) {

  if (isFALSE('formula' %in% class(formula))) formula <- as.formula(formula)

  fm <- Formula::Formula(formula)

  ## check if N is provided
  if (length(fm)[1] == 2) {
    ## obtain variable names
    outcome_name <- all.vars(formula(fm, lhs = 1, rhs = 0))
    trials_name  <- all.vars(formula(fm, lhs = 2, rhs = 0))

    ## obtain variable values
    outcome <- dplyr::pull(data, !!sym(outcome_name))
    trials  <- dplyr::pull(data, !!sym(trials_name))
    if (!any(c("numeric", "integer") %in% class(trials))) {
      stop("'trials' variable should be numeric.")
    } else {
      trials <- as.numeric(trials)
    }
  } else {
    ## only outcome
    outcome_name <- all.vars(fm)[1]
    outcome <- as.numeric(dplyr::pull(data, !!sym(outcome_name)))
    trials  <- NULL
  }

  if (!any(c("numeric", "integer") %in% class(outcome))) {
    ## check if the outcome variable is numeric
    stop("Outcome variable should be numeric.")
  }


  ## prepare X
  Xlist <- list()
  Xlist[['intercept']] <- rep(1, nrow(data))

  x_names <- all.vars(formula(fm, lhs = 0))
  for (i in seq_along(x_names)) {
    x_tmp <- dplyr::pull(data, !!sym(x_names[i]))
    if ('factor' %in% class(x_tmp)) {
      x_formula <- as.formula(paste("~ - 1 + ", x_names[i]))
    } else if (any(c('numeric', 'integer') %in% class(x_tmp))) {
      x_formula <- as.formula(paste("~ -1 + as.factor(", x_names[i], ")", sep = ""))
    } else if ('character' %in% class(x_tmp)) {
      x_formula <- as.formula(paste("~ -1 + as.factor(", x_names[i], ")", sep = ""))
      ## change variable names
    } else {
      stop('Variable', x_names[i], 'is not in a supported variable type.')
    }
    Xlist[[x_names[i]]] <- Matrix(model.matrix(x_formula, data = data))
  }

  ## compute number of observations in each categor / factor
  nj <- purrr::map(Xlist[-1], ~colSums(.x))

  ## create p_vec
  p_vec <- c('intercept' = 1, sapply(nj, length))

  ## compute regularization weights
  wj <- al_compute_weights(nj, p_vec)

  Xdesign <- do.call("cbind", Xlist)


  if (isTRUE(adaptive_weight)) {
    wj <- al_compute_adaptive_weights(outcome, Xdesign, trials, p_vec, wj)
  }

  return(list(y = outcome, trials = trials, X = Xdesign, nj = nj, pvec = p_vec, wj = wj))
}


al_compute_adaptive_weights <- function(y, X, trials, pvec, wj) {
  ## fit anova_logit without regularization
  fit <- anova_logit(
    y = y,
    X = X,
    trials = trials,
    option = list(regularize = FALSE, pvec = pvec)
  )

  ## obtain coefficients
  coefs <- fit$coef
  nobs  <- sum(trials)

  ## compute adaptive weights
  lb <- 2; ub <- 1
  for (j in 2:length(wj)) {
    pj <- pvec[j]
    dj <- pj * (pj - 1) / 2

    ub <- ub + pj
    betaj <- coefs[lb:ub]
    lb <- lb + pj
    iter <- 1
    for (p1 in 1:(pvec[j]-1)) {
      for (p2 in (p1+1):pvec[j]) {
        wj[[j]][iter] <- wj[[j]][iter] / (sqrt(nobs) * abs(betaj[p1] - betaj[p2]))
        iter <- iter + 1
      }
    }
  }

  return(wj)

}

#' Initialize coefficients
#' @keywords internal
al_params_initialize <- function(X, params) {
  params[[1]]$beta <- rnorm(ncol(X), 0, 0.5)
  return(params)
}


#' Set default opiions
#' @keywords internal
al_set_option <- function(option) {
  ## set maximum number of iterations
  if (isFALSE(exists("max_iter", option))) option$max_iter <- 200

  ## set tolerance parameter for the convergence
  if (isFALSE(exists('tol', option))) option$tol <- 1e-5

  ## set regularization options
  if (isFALSE(exists('regularize', option))) option$regularize <- FALSE

  ## set the default regularization parameter
  if (isFALSE(exists('lambda', option))) option$lambda <- 1

  return(option)
}



#' Expand design matrix
#' @keywords internal
al_expand_X <- function(X, p_vec, regularize) {
  ## no expansion when regularize = FALSE
  if (isFALSE(regularize)) {
    return(X)
  }

  ##
  ## Augment the original design matrix with zeros:
  ## - Orginal design matrix: X[j] with dimension p[j]
  ## - Augmented matrix: Xe[j] with dimension p[j] + 2 * d[j]
  ##   where d[j] = p[j] * (p[j] - 1) / 2
  ##   by augmenting Xe[j] = [X[j], 0] where 0 has dimension n by d[j]
  ##

  Xe  <- vector("list", length(p_vec))
  nobs <- nrow(X)
  ## intercept is not expanded
  Xe[[1]] <- X[,1]

  ## expand other variables
  lb <- 2; ub <- 1 + p_vec[2]
  for (j in 2:length(p_vec)) {
    dj <- p_vec[j] * (p_vec[j] - 1) / 2
    Xe[[j]] <- cbind(X[,lb:ub], Matrix::Matrix(0, nrow = nobs, ncol = 2 * dj))
    lb <- lb + p_vec[j]
    if (j < length(p_vec)) ub <- ub + p_vec[j+1]
  }

  ## reduce to matrix
  Xe <- do.call(cbind, Xe)
  return(Xe)
}




#' Define inputs for constrains
#' @param pvec A vector of length K where each element corresponds to the level of k-th variable.
#' @param regularize A boolena argument.
al_set_opt_input <- function(p_vec, regularize, lambda = 1, wj = 1) {
  if (isTRUE(regularize)) {
    ##
    ## Regularized problem
    ##    min (1/2) * b'Db - b'd
    ##    st   lb <= A'b <= ub
    ## Example
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

    ## create Amat
    Lj <- Sj <- Pj <- vector("list", length(p_vec))
    Lj[[1]] <- Sj[[1]] <- Pj[[1]] <- Matrix::Matrix(0, nrow = 1, ncol = 1)

    for (j in 2:length(p_vec)) {
      ## creat Dj
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
    Sj_block <- do.call(cbind, Sj)
    Pj_block <- Matrix::bdiag(Pj)[-1,]
    Amat     <- rbind(Lj_block, Sj_block, Pj_block)


    ##
    ## Define: lb <= Ax <= ub
    ##
    epsilon <- 0
    ub <- rep(epsilon, nrow(Lj_block))    #* Lj * b = 0
    lb <- rep(-epsilon, nrow(Lj_block))   #* Lj * b = 0
    lb <- c(lb, 0)                        #* lb for Sj = 0
    ub <- c(ub, lambda)                   #* ub for Sj = lambda
    lb <- c(lb, rep(0, nrow(Pj_block)))   #* positivity constrains on theta+-
    ub <- c(ub, rep(Inf, nrow(Pj_block))) #* no upper bound

  } else {
    ##
    ## No-regularization
    ##
    ## Example:
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
    ##

    ## impose sum to zero constraints
    Amat <- create_Amat_unreg(pvec = p_vec);

    ## intercept is constraint free
    Amat[1,1] <- 0

    # linear equality constraint (sum to zero)
    lb <- ub <- rep(0, length(p_vec))
  }

  ##
  ## set of constrains
  ##
  opt_input <- list(Amat = Amat, lb = lb, ub = ub)

  return(opt_input)
}



al_compute_weights <- function(nj, p_vec) {
  ##
  ## Compute weights of the form
  ##  wj[k~m] = sqrt(nj[k] + nj[m]) / (p_vec[j] + 1)
  ##

  ## no weights for the intercept
  wj_list <- vector("list", length = length(p_vec))
  wj_list[[1]] <- 1
  for (j in 2:length(p_vec)) {
    ## factor levels
    pj <- p_vec[j]
    dj <- pj * (pj - 1) / 2
    wj <- vector('double', length = dj)

    ## pair-wise counts
    iter <- 1
    for (p1 in 1:(p_vec[j]-1)) {
      for (p2 in (p1+1):p_vec[j]) {
        n1 <- nj[[j-1]][p1]
        n2 <- nj[[j-1]][p2]
        wj[iter] <- sqrt(n1 + n2) / (pj + 1)
        iter <- iter + 1
      }
    }

    wj_list[[j]] <- wj
  }

  return(wj_list)
}
