source('Q2_compute_error.R')

generate_plot <- function() {
  p <- 10
  training_RSS <- rep(0, p)
  test_RSS <- rep(0, p)
  
  for (exp_no in 1:100) {
    error_data <- compute_error(N_tr = 30)
    new_tr_RSS <- error_data[[1]]
    new_te_RSS <- error_data[[2]]
    training_RSS <- training_RSS + new_tr_RSS
    test_RSS <- test_RSS + new_te_RSS
  }
  
  training_RSS <- training_RSS / 100
  test_RSS <- test_RSS / 100
  
  j <- 1:10
  
  plot(j, training_RSS,
       main = 'RSS against Model Size',
       xlab = 'j', ylab = 'RSS',
       type = 'b',
       col = 'blue',
       ylim = c(0.6, 1.8))
  lines(j, test_RSS,
        type = 'b',
        col = 'red')
  legend('bottomleft',
         legend = c('Training Data', 'Test Data'),
         fill = c('blue','red'),
         bty = 'o') # bty = 'n' means no box around legend
}
