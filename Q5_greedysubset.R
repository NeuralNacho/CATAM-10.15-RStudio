# This will be the amended version of Q4_greedysubset.R
# as specified by the question

Q5_greedysubset <- function(T) {
  Y <- T[[1]]
  X <- T[[2]]
  p <- ncol(X)
  N <- nrow(X)
  
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
    
    min_RSS <- Inf
    best_beta_M <- c()  # Empty vector for now
    best_M <- c()
    
    for (M in search_space) {
      X_M <- X[, M]
      beta_M <- solve(t(X_M) %*% X_M, t(X_M) %*% Y)
      RSS <- t(Y - X_M %*% beta_M) %*% 
        (Y - X_M %*% beta_M)
      if (RSS < min_RSS) {
        best_beta_M <- beta_M
        best_M <- M
        min_RSS <- RSS
      }
    }
    # F-test added here:
    if (j >= 2) { # Cannot do F-test when j = 1
      F_quantile <- qf(0.95, 1, N - j - 1)
      test_stat <- (prev_min_RSS - min_RSS) / (min_RSS / (N - j - 1))
      print(paste('test stat', test_stat))
      print(paste('F quantile', F_quantile))
      if (test_stat < F_quantile) { 
        # < ! because if this is the case then d+1 is not
        # significant improvement
        break # escapes loop for (j in 1:p)
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
    prev_min_RSS <- min_RSS # Store for F-test
  }
  return(B)
}

# Code to run (in terminal) for example output:
# source('Q2_simulate_dataset.R')
# source('Q5_greedysubset.R')
# T <- simulate_dataset(60) 
# (For larger N can be more certain about components of beta)
# B <- Q5_greedysubset(T)
# View(B)

