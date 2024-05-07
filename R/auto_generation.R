#' Assessment Data Generation
#' @importFrom stats rnorm runif 
#' @import readxl
#' 
#' @param n The number of observations to generate.
#' @param seed Set the seed for randomness check
#' 
#' @return A dataframe of simulated data with columns corresponding to variables and a 'Race' column.
#' @export 
#'
#' @examples
#' simulated_data <- auto_generation(n = 100, seed = NULL)

auto_generation <- function(n = 100, seed = NULL){
  # Setting seed for reproducibility
  if (!is.null(seed)){
    set.seed(seed)
  } else {
    set.seed(sample.int(1000, 1))
  }
  
  # Define the path to the Excel files within the package
  path_corr <- system.file("extdata", "Correlation.xlsx", package = "AssessmentGen")
  path_bwai <- system.file("extdata", "black_white.xlsx", package = "AssessmentGen")
  
  # Importing correlation data
  correlation_matrix <- read_excel(path_corr)
  b_w_ai <- read_excel(path_bwai)

  # Generating random data from normal distribution
  col_num <- ncol(correlation_matrix)
  X <- matrix(rnorm(n * col_num), nrow = n, ncol = col_num)
  
  # Generating means and sds
  means <- rep(5, col_num)
  sd <- rep(1, col_num)
  
  # Apply means and standard deviations
  for (i in 1:ncol(X)) {
    X[, i] <- X[, i] * sd[i] + means[i]
  }
  
  # Generating outcome data
  choleski_matrix <- chol(correlation_matrix)
  Y <- X %*% choleski_matrix
  Y <- as.data.frame(Y)
  
  # Assigning race
  race <- ifelse(runif(n) > 0.5, "White", "Black")
  Y$Race <- race
  
  # Adjust means based on effect size for the Black group
  for (i in 1:length(b_w_ai)) {
    Y[Y$Race == "Black", i] <- Y[Y$Race == "Black", i] + b_w_ai[i]
  }
  
  # Getting column names
  column_names <- colnames(correlation_matrix)
  colnames(Y) <- c(column_names, "Race")
  
  return(Y)
}