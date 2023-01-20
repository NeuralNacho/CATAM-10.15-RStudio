source('Q2_compute_error.R')

generate_plot <- function() {
  p <- 10
  training_RSS <- rep(0, p)
  test_RSS <- rep(0, p)
  
  for (exp_no in 1:100) {
    error_data <- compute_error()
    new_tr_RSS <- error_data[[1]]
    new_te_RSS <- error_data[[2]]
    training_RSS <- training_RSS + new_tr_RSS
    test_RSS <- test_RSS + new_te_RSS
  }
  
  training_RSS <- training_RSS / 100
  test_RSS <- test_RSS / 100
  
  j <- 1:10
  
  plot()
  #     main = 'RSS against Model Size',
   #    xlab = 'j', ylab = 'RSS',
    #   col = 'blue')
  # lines(test_RSS, ... )

}