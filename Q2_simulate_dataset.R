# Personal access token: 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

library(MASS)

simulate_data <- function(N) {
  sigma_sq <- 1
  p <- 10
  beta = c(-0.5, 0.45, -0.4, 0.35, -0.3, 
           0.25, -0.2, 0.15, -0.1, 0.05)
  
  epsilon_vec <- rnorm(N, 0, sigma_sq)
  X_matrix <- mvrnorm(N, rep(0, p), diag(p))
  Y_vec <- X_matrix %*% beta + epsilon_vec
  
  return(Y_vec)
}