

## 
cbsw_interaction <- function() {
  ## prepare inputs 
  w <- weights * lambda / rho 
  
  ## update eata 
  params$eta <- cbsw_inter_update_eta()
  
  ## update phi 
  params$phi <- cbsw_inter_update_phi(eta, constMat, uvec, w)
  
  ## update u 
  params$u  <- cbsw_inter_update_u(eta, phi, uvec, constMat)  
}



cbsw_inter_update_eta <- function(const_obj, data_obj, params, controls) {
  
  fit <- optim(par      = params$eta, 
               fn       = cbsw_inter_loss,
               Xmat     = data_obj$X, 
               x_pop    = data_obj$population,
               Mmat     = const_obj$M, 
               constMat = const_obj$constMat, 
               uvec     = params$uvec,
               phi      = params$phi,
               rho      = constrols$rho,
               method   = "BFGS"
  )
  
  return(fit$par)
  
}

cbsw_inter_loss <- function(par, Xmat, x_pop, Mmat, constMat, uvec, phi, rho) {
  
  ## main objective component 
  const <- t(x_pop) %*% par 
  main  <- log(sum(exp(Xmat %*% par)))
  
  ## regularization component 
  q2 <- par %*% Mmat %*% par * rho / 2
  q1 <- rowSums(sapply(1:length(uvec), function(i) {
                        unew <- uvec[[i]] - phi[[i]]
                        as.vector(t(unew) %*% constMat[[i]]) 
                      })) %*% par * rho 
  return(as.vector(q1 + q2 + main - const))
}


#' Compute gradient of the loss function 
#' @keywords internal
cbsw_inter_loss_gradient <- function(par, Xmat, x_pop, Mmat, constMat, uvec, phi, rho) {
  m <- rowSums(sapply(1:length(uvec), function(i) {
                        unew <- uvec[[i]] - phi[[i]]
                        as.vector(t(unew) %*% constMat[[i]]) 
                      }))
  expXb <- exp(Xmat %*% par)
  main <- t(expXb) %*% Xmat / sum(expXb)
  grad <- (main - x_pop) + rho * (par %*% Mmat + m)
  return(as.vector(grad))
}



cbsw_inter_update_phi <- function(eta, constMat, uvec, w = 1) {
  ## compute y 
  tmp <- lapply(1:length(constMat), function(i) constMat[[i]] %*% eta + uvec[[i]])
  
  ## update phi 
  phi_new <- map(tmp, ~ as.vector(gSoftThreshold(.x, w)))

  ## first const is sum-to-zero: no phi is defined 
  phi_new[[1]] <- rep(0, length(phi_new[[1]]))
  
  return(phi_new)
}

gSoftThreshold <- function(y, t) {
  threshold <- max(0, 1 - t / sqrt(sum(y^2)))
  ynew <-  y * threshold
  return(ynew)  
}


#' Prepare Input Matrices 
#' @param level_main  A list of levels for main terms.
#' @param level_inter A list of levels for interaction terms.
#'                    Each element of the list contains to items: 
#'                     (1) level of the interaction term,
#'                     (2) corresponding main terms.
#' @examples 
#' level_main <- list(1, 3, 3, 2) 
#' level_inter <- list(
#'   list(9, c(2, 3)),
#'   list(6, c(2, 4))
#' )
cbsw_inter_input <- function(level_main, level_inter) {
  
  ## common objects 
  sparse_zeros <- lapply(level_main, function(i)
                          Matrix(0, nrow = 1, ncol = i, sparse = TRUE))
  sparse_ones <- lapply(level_main, function(i)
                          Matrix(1, nrow = 1, ncol = i, sparse = TRUE))  
  ## main terms 
  Dk <- list()
  Di <- list()
  Dk[[1]] <- Matrix(1, 1, 1, sparse = TRUE)
  for (i in 2:length(level_main)) {
    ## extract levels 
    Lk        <- level_main[[i]]

    ## Create differencing matrix
    Adj       <- matrix(1, nrow = Lk, ncol = Lk)
    tmp_graph <- igraph::graph_from_adjacency_matrix(Adj, mode = 'undirected', diag = FALSE)
    Dk[[i]]   <- genlasso::getDgSparse(tmp_graph)    
    
    ## prepare total difference matrix 
    ## add main term diff's 
    Di[[i]]   <- list()
    sparse_tmp <- rep(list(sparse_zeros), nrow(Dk[[i]]))
    for (j in 1:nrow(Dk[[i]])) {
      sparse_tmp[[j]][[i]] <- Dk[[i]][j, , drop = FALSE]
      ## create block-diagonal with empty rows 
      ## to make cols to be total number of parameters
      Di_block <- bdiag(sparse_tmp[[j]])
      ## remove empty rows 
      Di[[i]][[j]] <- Di_block[unique(Di_block@i)+1, , drop = FALSE]
    }
  }

  DD <- bdiag(Dk)
  id_group <- 0:(nrow(DD)-1)
  
  sparse_zeros_inter <- lapply(level_inter, function(i) {
    Matrix(0, nrow = 1, ncol = i[[1]], sparse = TRUE)
  })
  sparse_ones_inter <- lapply(level_inter, function(i) {
    Matrix(1, nrow = 1, ncol = i[[1]], sparse = TRUE)
  })
  
  
  Di[[1]] <- list(append(sparse_ones, sparse_ones_inter))
  
  ## interaction terms 
  for (i in 1:length(level_inter)) {
    Lk <- level_inter[[i]][[1]]
    A  <- level_inter[[i]][[2]][1]  ## second element is the index to the main 
    B  <- level_inter[[i]][[2]][2]  ## second element is the index to the main 
    idx_mat <- expand.grid(1:level_main[[A]], 1:level_main[[B]])
    
    
    Adj       <- matrix(1, nrow = Lk, ncol = Lk)
    tmp_graph <- igraph::graph_from_adjacency_matrix(Adj, mode = 'undirected', diag = FALSE)
    Dkl       <- genlasso::getDgSparse(tmp_graph)    
    tmp <- t(apply(Dkl, 1, function(x) which(x != 0)))
    group_mat <- cbind(idx_mat[tmp[,1], ], idx_mat[tmp[,2], ])
    
    for (j in 1:nrow(Dk[[A]])) {
      non_zero <- which(Dk[[A]][j,] != 0)
      use_id  <- (group_mat[,1] == non_zero[1]) &
                  (group_mat[,3] == non_zero[2]) & 
                  (group_mat[,2] == group_mat[,4])
      ## can be slow 
      ## add diff matrix about the interaction parameters
      ## A is always area indicator 
      Di[[A]][[j]] <- append(Di[[A]][[j]], Dkl[use_id, ])
    }
    
    for (j in 1:nrow(Dk[[B]])) {
      non_zero <- which(Dk[[B]][j,] != 0)
      use_id  <- (group_mat[,2] == non_zero[1]) &
                  (group_mat[,4] == non_zero[2]) & 
                  (group_mat[,1] == group_mat[,3])
      ## add diff matrix about the interaction parameters
      tmp          <- sparse_zeros_inter
      tmp[[i]]     <- Dkl[use_id, ]      
      Di[[B]][[j]] <- append(Di[[B]][[j]], tmp)      
    }
  }
  
  
  ## combine Di 
  constMat <- unlist(purrr::map(1:length(Di), function(i) purrr::map(Di[[i]], ~bdiag(.x))))
  
  ## adjust the intercept in the zero-sum const 
  constMat[[1]] <- constMat[[1]][-1, ]
  
  return(constMat)
}


  # Mmat <- Reduce("+", purrr::map(constMat, ~t(.x) %*% .x))



cbsw_inter_update_u <- function(eta, phi, uvec, constMat) {
  unew <- lapply(1:length(uvec), function(i) {
      uvec[[i]] + as.vector(constMat[[i]] %*% eta - phi[[i]])
  })
  return(unew)
}
