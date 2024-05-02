#' Assessment Data Generation
#' @importFrom stats rnorm runif
#'
#' @param correlation_matrix A square matrix specifying the desired correlations between variables.
#' @param n The number of observations to generate.
#' @param means A numeric vector specifying the mean for each variable.
#' @param sd A numeric vector specifying the standard deviation for each variable.
#' @param effect_size A numeric vector of effect sizes to simulate adverse impacts between groups.
#' 
#' @return A dataframe of simulated data with columns corresponding to variables and a 'Race' column.
#' @export 
#'
#' @examples
#' Omega <- as.data.frame(matrix(c(1, 0.8, 0.3, 0.8, 1, 0.8, 0.3, 0.8, 1), byrow = TRUE, ncol = 3))
#' name <- c("GMA", "Personality", "Performance")
#' colnames(Omega) <- name
#' means <- c(5, 5, 5)
#' sd <- c(1, 1, 1)
#' effect_size <- c(1.5, 0.3, 0)
#' simulated_data <- assessment_gen(Omega, n = 1000, means = means, sd = sd, effect_size)
#' 
assessment_gen <- function(correlation_matrix, n = 100, means, sd, effect_size){
  # Setting seed for reproducibility
  set.seed(42)
  
  # Creating Cholesky matrix
  choleski_matrix <- chol(correlation_matrix)
  
  # Generating random data from normal distribution
  col_num <- ncol(correlation_matrix)
  X <- matrix(rnorm(n * col_num), nrow = n, ncol = col_num)
  
  # Apply means and standard deviations
  for (i in 1:ncol(X)) {
    X[, i] <- X[, i] * sd[i] + means[i]
  }
  
  # Generating outcome data
  Y <- X %*% choleski_matrix
  Y <- as.data.frame(Y)
  
  # Assigning race
  race <- ifelse(runif(n) > 0.5, "White", "Black")
  Y$Race <- race
  
  # Adjust means based on effect size for the Black group
  for (i in 1:length(effect_size)) {
    Y[Y$Race == "Black", i] <- Y[Y$Race == "Black", i] + effect_size[i]
  }
  
  # Getting column names
  column_names <- colnames(correlation_matrix)
  colnames(Y) <- c(column_names, "Race")
  
  return(Y)
}