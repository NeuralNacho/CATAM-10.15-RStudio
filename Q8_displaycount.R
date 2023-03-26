# This file will count the number of times the four
# random variables are included in the model
source('Q8_getresults.R')

Q8_displaycount <- function() {
  Q5_greedy_count <- c()
  bestsubset_count <- c()
  greedy_count <- c()
  lars_count <- c()
  # vector for number of zeros in last 4 entries for each trial
  
  Q5_greedy_tally <- numeric(12)
  # ^ counts number of times each variable is included in a 
  # run of Q5_greedy
  Q5_greedy_values <- numeric(12)
  # adds all the Q5_greedy vectors together 
  
  for (i in 1:10) {
    print(i)
    errors <- Q8_getresults()
    Q5_greedy <- errors[[7]]
    bestsubset_crossval <- errors[[8]]
    greedysubset_crossval <- errors[[9]]
    lars_crossval <- errors[[10]]
    
    Q5_greedy_tally <- Q5_greedy_tally + (Q5_greedy != 0)
    Q5_greedy_values <- Q5_greedy_values + Q5_greedy
    

    Q5_greedy_count[i] <- length(which(Q5_greedy[9:12] != 0))
    bestsubset_count[i] <- length(which(
                            bestsubset_crossval[9:12] != 0))
    greedy_count[i] <- length(which(
                            greedysubset_crossval[9:12] != 0))
    lars_count[i] <- length(which(lars_crossval[9:12] != 0))
    # ^ counts number of non-zero elements in last four 
    # components

    # cat(Q5_greedy, '\n')
    # cat(bestsubset_crossval, '\n')
    # cat(greedysubset_crossval, '\n')
    # cat(lars_crossval, '\n')
    # ^ used in the figure in Q8 which shows outputted models
    # uncomment it to use
  }
  print( c( var(Q5_greedy_count), var(bestsubset_count), 
           var(greedy_count), var(lars_count) ) )
  print( c( mean(Q5_greedy_count), mean(bestsubset_count), 
            mean(greedy_count), mean(lars_count) ) )
  
  print(Q5_greedy_tally)
  print(Q5_greedy_values / 100)
  print(sum(Q5_greedy_tally) / 100)
}

