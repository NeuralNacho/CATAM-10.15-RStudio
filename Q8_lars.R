# An amended version of the 'lars' function from the 'lars'
# package in R which will work as input into Q6_crossval.R

library('lars')

Q8_lars <- function(T) {
  Y <- T[[1]]
  X <- T[[2]]
  object <- lars(X, Y, type = 'lasso')
  coef_matrix <- predict.lars(object, type = 'coef')$coefficients
  B <- coef_matrix[-1,]
  B <- t(B)
  return(B)
}
