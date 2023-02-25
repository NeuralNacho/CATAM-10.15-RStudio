greedysubset <- function(T) {
  Y <- T[[1]]
  X <- T[[2]]
  p <- ncol(X)
  
  B <- matrix(nrow = p, ncol = 0)
  prev_model <- c()
  
  for (j in 1:p) {
    search_space <- list()
    for (l in 1:p) {
      if (!is.element(l, prev_model)) {
        search_space[[length(search_space) + 1]] <- 
          c(prev_model, l)
      }
    }
    
    # Same code as Q3_bestsubset.R :
    min_RSS <- Inf
    best_beta_M <- c()  # Empty vector for now
    best_M <- c()
    
    for (M in search_space) {
      X_M <- X[, M]
      beta_M <- (solve(t(X_M) %*% X_M) 
                 %*% t(X_M) %*% Y)
      RSS <- t(Y - X_M %*% beta_M) %*% 
        (Y - X_M %*% beta_M)
      if (RSS < min_RSS) {
        best_beta_M <- beta_M
        best_M <- M
        min_RSS <- RSS
      }
    }
    
    new_col <- rep(0, p)
    beta_index = 1
    for (index in best_M) {
      new_col <- replace(new_col, index, best_beta_M[beta_index])
      beta_index <- beta_index + 1
    }
    B <- cbind(B, new_col)
    
    prev_model <- best_M  # Update best M for loop
  }
  return(B)
}

