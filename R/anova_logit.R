


##
## ANOVA logit
##

#' Logistic regression with ANOVA style parametrization.
#' @param y A vector of binary responses. In the binomial response, this corresponds to "success".
#' @param X A matrix of overparametrized
#' @param trials A vector of trial counts. In the binary outcome this is left as \code{NULL}.
#'  The lengh of this vector should match the length of \code{y}.
#' @param option A list of options. \code{pvec} should be provided.
#' @export
anova_logit <- function(y, X, trials = NULL, option = list()) {

  ## -------------------------------------------
  ## set the default value of the option parameters
  option <- al_set_option(option)

  ## EM ----------------------------------------
  params <- al_em_run(y, X, trials, option)

  ## fitted value ------------------------------
  betas  <- params$coefficient
  params$fitted <- 1 / (1 + exp(-as.vector(X %*% betas)))

  ## return estimates --------------------------
  attr(params, "option") <- option
  class(params)          <- c("anova_logit")
  return(params)
}


#' EM-algorithm for the ANOVA logit
#' @keywords internal
al_em_run <- function(y, X, trials, option) {

  ## initialize parameters
  params <- vector("list", length = option$max_iter)
  params <- al_params_initialize(X, params)

  ## define constrains
  opt_input <- al_set_opt_input(option$pvec, option$regularize, option$lambda)

  if (isTRUE(option$regularize)) {
    Xe <- al_expand_X(X, option$pvec)
  } else {
    Xe <- X
  }

  # set trials
  if (is.null(trials)) trials <- rep(1, length(y))

  ##
  ## Run EM algorithm
  ##
  for (iter in seq_len(option$max_iter)) {
    ## E-step ---------------------------------------------
    params[[iter]]$omega <- al_estep(X, params[[iter]]$beta)

    ## M-step ---------------------------------------------
    params[[iter+1]]$beta <- al_mstep(y, Xe, trials, params[[iter]]$omega, opt_input)

    ## discard slack variables ----------------------------
    if (isTRUE(option$regularize)) {
      params[[iter+1]]$beta <- al_subset_beta(params[[iter+1]]$beta, option$pvec)
    }

    ## check_convergence ----------------------------------
    if (iter > 1 &&
        al_check_convergence(params[c(iter - 1, iter)]) < option$tol
      ) {
        break;
    }
  } ## end of estimation iterations

  ## remve NULL elements and returns the final estimate
  params <- params[purrr::map_lgl(params, ~!is.null(.x))]
  par    <- vector("list")
  par$coefficient <- params[[length(params)]][['beta']]
  par$iteration   <- iter
  par$convergence <- if_else(iter < option$max_iter, 1, 0)
  return(par)
}


##
## EM functions
##

#' E-step function to compute the expectation of omega.
#' @keywords internal
al_estep <- function(X, betas) {
  Xb    <- X %*% betas
  omega <- as.vector(tanh(Xb / 2) / (2 * Xb))
  return(omega)
}

#' M-step function to solve quadratic programming.
#' @import Matrix
#' @keywords internal
al_mstep <- function(y, X, trials, omega, opt_input) {
  ## prepare inputs
  XO   <- t(X) %*% diag(omega)
  Dmat <- XO %*% X
  dvec <- as.vector(XO %*% ((y - trials / 2) / omega))

  Amat   <- opt_input$Amat
  lb_vec <- opt_input$lb
  ub_vec <- opt_input$ub

  ## solve QP
  ## can we pass the initial base?
  settings <- osqp::osqpSettings(verbose = FALSE, eps_abs = 1e-5, eps_rel = 1e-5)
  fit      <- osqp::solve_osqp(Dmat, -dvec, Amat, l = lb_vec, u = ub_vec, pars = settings)

  return(fit$x)
}


##
## Handling inputs
##

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

#' Expand design matrix
#' @keywords internal
al_expand_X <- function(X, p_vec) {
  ##
  ## Expand the original design matrix X[j] with dimension p[j] to
  ## Xe[j] with dimension p[j] + 2 * d[j] where d[j] = p[j] * (p[j] - 1) / 2
  ##  by augmenting Xe[j] = [X[j], 0] where 0 has dimension n by d[j]
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
al_set_opt_input <- function(p_vec, regularize, lambda = 1) {
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
    ##      | 0 , w2 , w3 | /* weighted sum of theta+ and theta -
    ##      | 0 , M2 , 0  | /* positivity constarints
    ##      | 0 , 0  , M3 | /* positivity constrains
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

      one_zero_j <- Matrix::Matrix(c(rep(1, p_vec[j]), rep(0, 2 * dj)), nrow = 1)
      Ij <- Matrix::Diagonal(dj)

      Lj[[j]] <- rbind(
        cbind(Dj, Ij, -Ij),
        one_zero_j
      )

      ## create theta+ + theta- <= t constraints
      weigths <- 1
      zero_ones <- Matrix::Matrix(c(rep(0, p_vec[j]),
                                    rep(weigths, 2 * dj)), nrow = 1)
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
    lb <- ub <- rep(0, nrow(Lj_block))    #* Lj * b = 0
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

  ## set of constrains
  opt_input <- list(Amat = Amat, lb = lb, ub = ub)

  return(opt_input)
}


#' Check convergence by computing the maximum rate of change
#' @keywords internal
al_check_convergence <- function(params) {
  ## compute max( |beta[t] - beta[t-1]| / |beta[t-1]| )
  max_diff <- max( abs(unlist(params[[1]]) - unlist(params[[2]])) /
                      abs(unlist(params[[1]])) )

  return(max_diff)
}

#' Initialize coefficients
#' @keywords internal
al_params_initialize <- function(X, params) {
  params[[1]]$beta <- rnorm(ncol(X), 0, 0.5)
  return(params)
}



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
