source('Q2_simulate_dataset.R')

compute_error <- function(N_tr = 30) {
  N_te = 1000
  p <- 10
  
  training_RSS <- vector() # empty vector
  test_RSS <- vector()
  
  training_data <- simulate_dataset(N_tr)
  Y_training <- training_data[[1]]
  X_training <- training_data[[2]]
  test_data <- simulate_dataset(N_te)
  Y_test <- test_data[[1]]
  X_test <- test_data[[2]]
  
  for (j in 1:p) {
    X_M <- X_training[,1:j]
    # columns 1 to j of X
    beta_M <- (solve(t(X_M) %*% X_M) 
                %*% t(X_M) %*% Y_training)
    # solve finds matrix inverse
    beta_M <- c(beta_M, integer(p - j))
    # integer(p - j) is p-j length vector of 0's
    training_RSS <- c(training_RSS, 
                      t(Y_training - X_training %*% beta_M) %*% 
                      (Y_training - X_training %*% beta_M) / N_tr)
    test_RSS <- c(test_RSS, 
                  t(Y_test - X_test %*% beta_M) %*% 
                  (Y_test - X_test %*% beta_M) / N_te)
    # each entry of these vectors is the RSS for j = 1,...,p
  }
  
  return(list(training_RSS, test_RSS))
}
