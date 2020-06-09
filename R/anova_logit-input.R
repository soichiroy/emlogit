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
    covar_name   <- all.vars(formula(fm, lhs = 0, rhs = 1))
    
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
    covar_name <- all.vars(fm)[-1]
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

  return(list(y = outcome, trials = trials, X = Xdesign, nj = nj, pvec = p_vec, wj = wj, covar_name = covar_name))
}


al_compute_adaptive_weights <- function(y, X, trials, pvec, wj) {
  ## fit anova_logit without regularization
  fit <- anova_logit.matrix(
    y = y,
    X = X,
    trials = trials,
    option = al_set_option(list(regularize = FALSE, pvec = pvec))
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
  
  ## set default option 
  option_def <- list(
    max_iter        = 200,
    tol             = 1e-5,
    regularize      = FALSE, 
    lambda          = 1,
    adaptive_weight = FALSE,
    wj              = NULL,
    pvec            = NULL,
    verbose         = FALSE 
  )
  nmsC <- names(option_def)

  ## overwrite the default by the user specific options 
  option_def[(namc <- names(option))] <- option 

  ## throw erros when unknown control is given 
  if (length(noNms <- namc[!namc %in% nmsC])) {
    warning("unknown names in control: ", paste(noNms, collapse = ", ")) 
  }
  
  return(option_def)
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
    ## with regularization 
    const_input <- create_const_regularized(p_vec, lambda, wj)
  } else {
    ## No-regularization
    const_input <- create_const_unregularized(p_vec)
  }

  return(const_input)
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
