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


#' Prepare Data
#' @import Formula
#' @importFrom rlang !! sym
#' @importFrom Matrix Matrix
#' @export
al_data <- function(formula, data) {

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


  Xdesign <- do.call("cbind", Xlist)

  return(list(y = outcome, trials = trials, X = Xdesign))
}
