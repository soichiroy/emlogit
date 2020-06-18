
##
## Covariate Balancing Survey Weights with Categorical Variables
##



#' Covariate Balancing Survey Weights for Categorical Variables
#' @export
CBSW <- function(formula, data, option) {


  ## prepare inputs
  data <- al_data(format, data)

  ## create constraints
  const <- create_input_matrix(as.list(data$pvec))

  ## initialize parameters
  params <- cbsw_prepare_params(const)

  ## optimization
  params <- cbsw_admm(params, data, const, option)
}


#' Create Matrix Inputs
#' @import Matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom genlasso getDgSparse
#' @keywords internal
create_input_matrix <- function(Lk_list) {
  ##
  ## ADMM formulation to solve the following problem
  ##
  ##         minimize     f(β) + g(η)
  ##        subject to    Aβ + Bη = c
  ##


  ## create A
  Ak <- D <- dvec <- list()
  Ak[[1]] <- Matrix(0, nrow = 1, ncol = 1)

  for (k in 2:length(Lk_list)) {
    Lk        <- Lk_list[[k]]

    ## Create differencing matrix
    Adj       <- matrix(1, nrow = Lk, ncol = Lk)
    tmp_graph <- igraph::graph_from_adjacency_matrix(Adj, mode = 'undirected', diag = FALSE)
    Dk        <- genlasso::getDgSparse(tmp_graph)

    ## Create ones and zeros
    ones  <- Matrix(1, nrow = 1, ncol = Lk, sparse = TRUE)

    ## Combine all
    ## Ak = | 1 |
    ##      | D |
    ##
    Ak[[k]]     <- rbind(ones, Dk)
    D[[k-1]]    <- Dk
    dvec[[k-1]] <- nrow(Dk)
  }

  Amat <- bdiag(Ak)

  ## create B
  ## B = | 0 |
  ##     | I |
  Bk <- list()
  Bk[[1]] <- Matrix(0, nrow = 1, ncol = 1)
  for (k in 2:length(Lk_list)) {
    Lk <- Lk_list[[k]]
    zeros_Lk  <- Matrix(0, nrow = 1, ncol = dvec[[k-1]])
    Bk[[k]] <- rbind(zeros_Lk, -Diagonal(dvec[[k-1]]))
  }

  Bmat <- bdiag(Bk)
  return(list(A = Amat, B = Bmat, Ak = Ak, Bk = Bk, D = D, d = dvec))
}



#' Prepare parameter Inputs
#' @param obj An output object from \code{create_input_matrix}.
#' @import Matrix
#' @keywords internal
cbsw_prepare_params <- function(obj) {

  ## Prepare β for f(β)
  ## need to impose sum to one constraints
  beta_param <- rnorm(ncol(obj$A))

  ## Prepare η for g(η)
  eta_param  <- rnorm(ncol(obj$B))

  ## Prepare w
  w_param <- rnorm(nrow(obj$A))

  return(list(beta = beta_param, eta = eta_param, w = w_param))
}


cbsw_admm <- function(params, data, const, option) {


  diff_obj <- 1
  for (iter in 1:option$max_iter) {

    ## update β
    params$beta <- cbsw_update_beta(params, data, const, option)

    ## update η
    params$eta <- cbsw_update_eta(params, const)

    ## update w
    params$w <- cbsw_update_w(params, const)

    if (diff_obj < option$tol) {
      break;
    }
  }

  return(params)

}


#' Estimate beta
#' @keywords internal
cbsw_update_beta <- function(params, data, const, option) {

  if (isTRUE(option$use_grad)) {
    fit <- optim(par = params$beta, fn = cbsw_main_objective_fn,
                 gr = cbsw_main_objective_gr,
                 S = data$y, X = data$X, params = params,
                 A = const$A, B = const$B, rho = const$rho,
                 method = "BFGS"
               )
  } else {
    fit <- optim(par = params$beta, fn = cbsw_main_objective_fn,
                 S = data$S, X = data$X, params = params,
                 A = const$A, B = const$B, rho = const$rho,
                 method = "BFGS"
               )
  }

  return(fit$par)
}


cbsw_main_objective_fn <- function(par, S, X, params, A, B, rho) {
  ## Objective
  ##
  ##    S * exp(X'β) + (1 - S) * X'β + ρ/2 * (β'A'Aβ + 2y'Aβ)
  ##
  ## where y = Bη + w
  ##

  ## construct y
  y <- B %*% params$eta + params$w

  ## compute Aβ
  Ab <- A %*% par

  ## compute Xβ
  Xb <- X %*% par

  ## evaluate the objective
  obj_main <- sum((-1) * S * exp(-Xb) -  (1 - S) * Xb)
  obj_pen  <- as.vector(rho/2 * (t(Ab) %*% Ab + 2 * t(y) %*% Ab))

  return(obj_main + obj_pen)
}


cbsw_main_objective_gr <- function(par, S, X, params, A, B, rho) {
  ## compute y
  y <- B %*% params$eta + params$w

  ## AA
  AA <- t(A) %*% A

  ## condition on S
  X1 <- X[S == 1, ]
  X0 <- X[S == 0, ]

  X1b <- X1 %*% par

  ## compute gradient
  gr1 <- t(X1) %*% exp(X1b)
  gr0 <- -colSums(X0)
  grp <- rho * AA %*% par + rho * t(y) %*% A
  gr  <- gr1 + gr0 + grp
  return(gr)
}

softThreshld <- function(x, lambda) {
  return(sign(x) * pmax(abs(x) - lambda, 0))
}


#' Update eta
#' @param params A list object of parameters.
#' @param A A matrix in the constrains.
#' @param lambda A scalar or a vector of regularization parameters.
#' @return A vector of η parameter in the objective.
#' @import Matrix
#' @keywords internal
cbsw_update_eta <- function(params, const) {
  ## form y
  y <- as.vector(const$A %*% params$beta  + params$w)

  ## update eta by soft-thresholding
  eta <- softThreshld(t(const$B) %*% y, const$lambda)
  return(eta)
}


#' Update scaled slack variable
#' @param params A list of parameter object.
#' @param A A matrix in the constraint. Aβ + Bη = c.
#' @param B B matrix in the constraint: Aβ + Bη = c.
#' @keywords internal
cbsw_update_w <- function(params, const) {
    w <- const$A %*% params$beta + const$B %*% params$eta + params$w
    return(w)
}
