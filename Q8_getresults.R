source('Q3_bestsubset.R')
source('Q4_greedysubset.R')
source('Q5_greedysubset.R')
source('Q6_crossval.R')
source('Q8_lars.R')
library('lars')

# Generating the dataset:

prostate_data <- read.table("~/CATAM-10.15-RStudio/II-10-15-2022-prostate.dat", 
                            quote="\"", comment.char="")

Q8_getresults <- function() {
  for (i in 1:4) {
    new_col <- rnorm(97)
    prostate_data <- cbind(prostate_data, new_col)
  }
  random_permutation <- sample(97)
  test_data_indices <- random_permutation[c(1:27)]
  test_data <- list(prostate_data[test_data_indices,1], 
                  data.matrix(prostate_data[test_data_indices,-1]))
  training_data <- list(prostate_data[-test_data_indices,1],
                  data.matrix(prostate_data[-test_data_indices,-1]))
  
  
  # Subset selection outputs:
  
  bestsubset_output <- bestsubset(training_data)
  greedysubset_output <- greedysubset(training_data)
  Q5_greedysubset_output <- Q5_greedysubset(training_data)
  
  
  # Shrinkage-based methods outputs:
  
  bestsubset_crossval <- crossval(training_data, bestsubset)
  # Notice that the above is equivalent to L0 norm Lasso
  greedysubset_crossval <- crossval(training_data, greedysubset)
  lars_crossval <- crossval(training_data, Q8_lars)
  # lars_crossval can be thought of as the L1 Lasso estimator which
  # performs best in terms of prediction error (i.e. we have selected
  # the value of lambda which gives this optimal estimator)
  
  
  # Use test data to compute error of all the methods above:
  
  Y_test <- test_data[[1]]
  X_test <- test_data[[2]]
  bestsubset_test_error <- c()
  for (i in 1:ncol(bestsubset_output)) {
    bestsubset_test_error[i] <- 
      t(Y_test - X_test %*% bestsubset_output[,i]) %*% 
      (Y_test - X_test %*% bestsubset_output[,i]) / 30
  }
  greedysubset_test_error <- c()
  for (i in 1:ncol(greedysubset_output)) {
    greedysubset_test_error[i] <- 
      t(Y_test - X_test %*% greedysubset_output[,i]) %*%
      (Y_test - X_test %*% greedysubset_output[,i]) / 30
  }
  Q5_greedysubset_test_error <- c()
  for (i in 1:ncol(Q5_greedysubset_output)) {
    Q5_greedysubset_test_error[i] <- 
      t(Y_test - X_test %*% Q5_greedysubset_output[,i]) %*% 
      (Y_test - X_test %*% Q5_greedysubset_output[,i]) / 30
  }
  
  bestsubset_crossval_test_error <- 
    t(Y_test - X_test %*% bestsubset_crossval) %*% 
    (Y_test - X_test %*% bestsubset_crossval) / 30
  greedysubset_crossval_test_error <- 
    t(Y_test - X_test %*% greedysubset_crossval) %*% 
    (Y_test - X_test %*% greedysubset_crossval) / 30
  lars_crossval_test_error <- 
    t(Y_test - X_test %*% lars_crossval) %*% 
    (Y_test - X_test %*% lars_crossval) / 30
  
  return(list(bestsubset_test_error, 
              greedysubset_test_error,
              Q5_greedysubset_test_error, 
              as.numeric(bestsubset_crossval_test_error),
              as.numeric(greedysubset_crossval_test_error),
              as.numeric(lars_crossval_test_error),
              Q5_greedysubset_output[,i], # These last four are for Q8_displaycount
              bestsubset_crossval,
              greedysubset_crossval,
              lars_crossval))
}





