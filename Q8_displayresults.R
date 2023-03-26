source('Q8_getresults.R')

Q8_displayresults <- function() {
  bestsubset_cv_error <- c()
  greedysubset_cv_error <- c()
  lars_cv_error <- c()
  Q5_greedy_end_error <- c() 
  # ^ will be error from final column of Q5_greedysubset
  for (i in 1:100) {
    print(i)
    errors <- Q8_getresults()
    # All of the test data RSS in a list
    bestsubset_error <- errors[[1]]  # Unused (not useful)
    greedysubset_error <- errors[[2]]  # Unused (not useful)
    Q5_greedysubset_error <- errors[[3]]
    bestsubset_cv_error[i] <- errors[[4]]
    greedysubset_cv_error[i] <- errors[[5]]
    lars_cv_error[i] <- errors[[6]]
    Q5_greedy_end_error[i] <- tail(errors[[3]], n = 1)
  }
  data <- data.frame(Bestsubset_Crossval = bestsubset_cv_error,
               Greedysubset_Crossval = greedysubset_cv_error,
               LARS_Crossval = lars_cv_error,
               Q5_Greedysubset = Q5_greedy_end_error)
  means <- colMeans(data)
  boxplot(data,
          xlab = 'Variable Selection Method',
          ylab = 'Test Error',
          col=c("indianred1","skyblue",
                "palegreen","lightgoldenrod"))
  points(x = 1:4,  # Add mean points to plot
         y = means,
         col = "black",
         pch = 16)
}