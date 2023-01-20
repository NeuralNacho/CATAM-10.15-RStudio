source('Q2_simulate_dataset.R')

compute_error <- function() {
  p <- 10
  
  training_RSS <- vector() # empty vector
  test_RSS <- vector()
  
  training_data <- simulate_dataset(30)
  X_training <- training_data[[1]]
  Y_training <- training_data[[2]]
  test_data <- simulate_dataset(1000)
  X_test <- test_data[[1]]
  Y_test <- test_data[[2]]
  
  beta_LS <- (solve(t(X_training) %*% X_training) 
              %*% t(X_training) %*% Y_training)
  # solve finds matrix inverse
  for (j in 1:p) {
    beta_M <- c(beta_LS[1:j], integer(p - j))
    # integer(p - j) is p-j length vector of 0's
    training_RSS <- c(training_RSS, 
                      t(Y_training - X_training %*% beta_M) %*% 
                      (Y_training - X_training %*% beta_M) / 30)
    test_RSS <- c(test_RSS, 
                  t(Y_test - X_test %*% beta_M) %*% 
                  (Y_test - X_test %*% beta_M) / 1000)
  }
  
  return(list(training_RSS, test_RSS))
}
