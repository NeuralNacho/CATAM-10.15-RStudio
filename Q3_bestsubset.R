get_search_space <- function(p) {
  # This function will generate all unique subsets of
  # {1,...,p} (power set)
  output <- list(c())
  for (i in 1:p) {
    output_copy <- output
    for (seq in output_copy) {
      output[[length(output) + 1]] <- c(seq, i)
    }
  }
  return(output)
}

bestsubset <- function(T) {
  # T is the data set. Assume it is in the format as returned
  # by Q2_simulate_dataset.R
  Y <- T[[1]]
  X <- T[[2]]
  p <- ncol(X)  # This is the length of x_i
  
  B <- matrix(nrow = p, ncol = 0)
  search_space <- get_search_space(p)
  
  for (j in 1:p) {
    # Setup :
    search_space_j <- list()
    for (seq in search_space) {
      if (length(seq) == j) {
        search_space_j[[length(search_space_j) + 1]] <- seq
      }
    }  # Taken all the length j sequences from the search space
    
    min_RSS <- Inf
    best_beta_M <- c()  # Empty vector for now
    best_M <- c()
    
    # Finding optimal beta:
    for (M in search_space_j) {
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
  
    # Make it a col vector of length p:
    new_col <- rep(0, p)
    beta_index = 1
    for (index in best_M) {
      new_col <- replace(new_col, index, best_beta_M[beta_index])
      beta_index <- beta_index + 1
    }
    
    # Append to matrix B
    B <- cbind(B, new_col)
  }
  return(B)
}

