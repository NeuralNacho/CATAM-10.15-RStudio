source('Q8_getresults.R')

Q8_displayresults <- function() {
  bestsubset_cv_error <- c()
  greedysubset_cv_error <- c()
  lars_cv_error <- c()
  Q5_greedy_end_error <- c() 
  # ^ will be error from final column of Q5_greedysubset
  for (i in 1:30) {
    errors <- Q8_getresults()
    # All of the test data RSS in a list
    bestsubset_error <- errors[[1]]  # USELESS BIASED
    greedysubset_error <- errors[[2]]  # USELESS BIASED
    Q5_greedysubset_error <- errors[[3]]
    bestsubset_cv_error[i] <- errors[[4]]
    greedysubset_cv_error[i] <- errors[[5]]
    lars_cv_error[i] <- errors[[6]]
    Q5_greedy_end_error[i] <- tail(errors[[3]], n = 1)
  }
  boxplot(list(bestsubset_cv_error, greedysubset_cv_error, 
               lars_cv_error, Q5_greedy_end_error))
}