
##
## Covariate Balancing Survey Weights with Categorical Variables
##



#' Covariate Balancing Survey Weights for Categorical Variables
#' @export
CBSW <- function(formula, data, option) {

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

  for (k in seq_len(length(Lk_list))) {
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
    Ak[[k+1]] <- rbind(ones, Dk)
    D[[k]]    <- Dk
    dvec[[k]] <- nrow(Dk)
  }

  Amat <- bdiag(Ak)

  ## create B
  ## B = | 0 |
  ##     | I |
  Bk <- list()
  for (k in seq_len(length(Lk_list))) {
    Lk <- Lk_list[[k]]
    zeros_Lk <- Matrix(0, nrow = 1, ncol = Lk)
    Bk[[k]]  <- rbind(zeros_Lk, -Diagonal(Lk))
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
  beta_param <- rnorm(ncol(obj$A))

  ## Prepare η for g(η)
  eta_param  <- rnorm(ncol(obj$B))


  return(list(beta = beta_param, eta = eta_param))
}


#' Format data for CBSW 
#' 
cbsw_data <- function(
  formula, data
) {
  
  ## call anova logit data format 
  dat <- al_data(format, data)
}



cbsw_admm <- function(option) {
  
  for (iter in 1:option$max_iter) {
    
    ## update β
    
    ## update η
    
    ## update residuals
    
    if (diff_obj < option$tol) {
      break;
    }
  }
  
  
}

cbsw_update_beta <- function() {
  
}


softThreshold <- function(x, lambda) {
  return(sign(x) * pmax(abs(x) - lambda, 0))
}


#' Update eta 
#' @import Matrix 
#' @keywords internal
cbsw_update_eta <- function(params, A, lambda) {
  ## form y 
  y <- as.vector(A %*% params$beta + params$w)
  
  ## update eta by soft-thresholding 
  eta <- softThreshold(y, lambda)
  return(eta)
}

cbsw_update_resid <- function() {
  
}
