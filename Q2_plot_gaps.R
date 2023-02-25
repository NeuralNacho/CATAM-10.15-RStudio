source('Q2_compute_error.R')

plot_gaps <- function() {
  Ntr = 200
  p <- 10
  gap <- rep(0, p)
  # have vector for gaps between test and training
  # RSS
  
  for (exp_no in 1:100) {
    error_data_Ntr <- compute_error(N_tr = Ntr)
    new_tr_RSS <- error_data_Ntr[[1]]
    new_te_RSS <- error_data_Ntr[[2]]
    
    gap <- gap + new_te_RSS - new_tr_RSS
  }
  
  gap <- gap / 100
  
  j <- 1:10
  
  plot(j, gap,
       main = 'Error gap Model Size',
       xlab = 'j', ylab = 'y',
       type = 'b',
       col = 'green',
       ylim = c(0, 0.1)) # EDIT for desired Ntr
  #ticks = c(-0.3, -0.2, -0.1, 0)
  #axis(side = 2, at = ticks)
  lines(j, 2*j/Ntr,
        type = 'l',
        col = 'cyan')
  legend('topleft',
         legend = c('RSS difference Ntr = 200', 
                    'y = 2j/200'),  # EDIT for desired Ntr
         fill = c('green', 'cyan'),
         bty = 'o') # bty = 'n' means no box around legend
}