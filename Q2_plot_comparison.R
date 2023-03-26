source('Q2_compute_error.R')

plot_comparison <- function() {
  p <- 10
  training_RSS_diff <- rep(0, p)
  test_RSS_diff <- rep(0, p)
  # have vectors for differences in RSS for
  # different values of Ntr
  
  for (exp_no in 1:100) {
    error_data_30 <- compute_error(N_tr = 30)
    error_data_200 <- compute_error(N_tr = 200)
    new_tr_RSS_30 <- error_data_30[[1]]
    new_te_RSS_30 <- error_data_30[[2]]
    new_tr_RSS_200 <- error_data_200[[1]]
    new_te_RSS_200 <- error_data_200[[2]]
    training_RSS_diff <- training_RSS_diff + 
      new_tr_RSS_30 - new_tr_RSS_200
    test_RSS_diff <- test_RSS_diff +
      new_te_RSS_30 - new_te_RSS_200
  }
  
  training_RSS_diff <- training_RSS_diff / 100
  test_RSS_diff <- test_RSS_diff / 100
  
  j <- 1:10
  
  plot(j, training_RSS_diff,
       main = 'RSS difference against Model Size',
       xlab = 'j', ylab = 'y',
       type = 'b',
       col = 'blue',
       ylim = c(-0.3, 0))
  #ticks = c(-0.3, -0.2, -0.1, 0)
  #axis(side = 2, at = ticks)
  #lines(j, test_RSS_diff, 
         # Don't actually want test RSS for this graph
  #      type = 'b',
  #      col = 'red')
  lines(j, -17 * j / 600,
        type = 'l',
        col = 'cyan')
  legend('topright',
         legend = c('Training RSS difference',
                    'y = -0.0283x'),
         fill = c('blue', 'cyan'),
         bty = 'o') # bty = 'n' means no box around legend
}
