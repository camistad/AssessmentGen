assessment_gen <- function(correlation_matrix, n = 100, mean, sd){
  # Setting seed
  set.seed(42)
  
  # Creating Choleski matrix
  choleski_matrix <- chol(correlation_matrix)
  
  # Generating random data from normal distribution
  col_num <- ncol(correlation_matrix)
  X <- matrix(rnorm(n*col_num), nrow = n, ncol = col_num)
  
  # Setting mean and sd
  mean <- rep(5, col_num)
  sd <- rep(1, col_num)
  
  # Transform data to specific means and sd
  for (i in 1:ncol(X)) {
    X[, i] <- X[, i] * sd[i] + mean[i]
  }
  
  # Generating outcome data
  Y <- X %*% choleski_matrix
}