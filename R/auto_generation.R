#' Assessment Data Generation
#' @importFrom stats rnorm runif 
#' @import readxl
#' @import dplyr
#' 
#' @param n The number of observations to generate.
#' @param seed Set the seed for randomness check
#' @param means User input means for the variable. If not provided, means of 5 is used
#' @param sds User input sd for the variable. If not provided, sd of 1 us used
#' 
#' @return A dataframe of simulated data with columns corresponding to variables and a 'Race' column.
#' @export 
#'
#' @examples
#' simulated_data <- auto_generation(n = 100, seed = NULL, means = NULL, sds = NULL)

auto_generation <- function(n = 100, seed = NULL, means = NULL, sds = NULL){
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
  correlation_matrix <- as.matrix(read_excel(path_corr))
  b_w_ai <- as.numeric(read_excel(path_bwai))
  
  # Number of observations for each race
  n_white <- floor(n / 2)
  n_black <- n - n_white
  
  # Generating random data for White participants
  col_num <- ncol(correlation_matrix)
  X_white <- matrix(rnorm(n_white * col_num), nrow = n_white, ncol = col_num)
  
  # Generating means and sd
  if (!is.null(means)) {
    means = means 
  } else {
    means <- rep(5, col_num)
  }
  means <- rep(5, col_num)
  
  if (!is.null(sds)) {
    sds = sds 
  } else {
    sds <- rep(1, col_num)
  }
  
  # Apply means and standard deviations to White data
  for (i in 1:ncol(X_white)) {
    X_white[, i] <- X_white[, i] * sds[i] + means[i]
  }
  
  # Generating outcome data for White participants
  cholesky_matrix <- chol(correlation_matrix)
  Y_white <- X_white %*% cholesky_matrix
  Y_white <- as.data.frame(Y_white)
  Y_white$Race <- "White"
  
  # Generating random data for Black participants
  X_black <- matrix(rnorm(n_black * col_num), nrow = n_black, ncol = col_num)
  
  # Apply means and standard deviations to Black data
  for (i in 1:ncol(X_black)) {
    X_black[, i] <- X_black[, i] * sds[i] + means[i]
  }
  
  # Generating outcome data for Black participants
  Y_black <- X_black %*% cholesky_matrix
  Y_black <- as.data.frame(Y_black)
  
  # Adjust means based on effect size for the Black group
  for (i in 1:col_num) {
    Y_black[, i] <- Y_black[, i] - b_w_ai[i]
  }
  
  Y_black$Race <- "Black"
  
  # Combine both datasets
  Y <- bind_rows(Y_white, Y_black)
  
  # Getting column names
  column_names <- colnames(correlation_matrix)
  colnames(Y)[1:col_num] <- column_names
  
  return(Y)
}

# TODO: include parameter
# TODO: do banding