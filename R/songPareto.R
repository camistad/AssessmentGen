songPareto <- function(data, predictors, sr, d = NULL, corrMat = NULL, Spac = 20, graph = T, 
                        display_solution = T) { 
  
  prop <- min(c((sum(data$Race == "Black") / sum(data$Race == "White")), 
                sum(data$Race == "White") / sum(data$Race == "Black")))
  
  if (!all(predictors %in% colnames(data))) {
    stop("One or more predictors are not in the data frame columns")
  }
  
  data <- data[, predictors, drop = FALSE]
  
  if(is.null(d)) {
    path_bwai <- system.file("extdata", "black_white.xlsx", package = "AssessmentGen")
    d <- read_excel(path_bwai)
  }
  
  if (!all(predictors %in% colnames(d))) {
    stop("One or more predictors are not in the difference vector")
  }
  
  d <- d[predictors, predictors, drop = FALSE]
  
  if(is.null(corrMat)) {
    path_corr <- system.file("extdata", "Correlation.xlsx", package = "AssessmentGen")
    corrMat <- read_excel(path_corr)
  }
  
  if (!all(predictors %in% colnames(corrMat))) {
    stop("One or more predictors are not in the correlation matrix columns")
  }
  
  corrMat <- corrMat[predictors, predictors, drop = FALSE]
    
  paretoR(prop, sr, d, corrMat, Spac, graph, display_solution)
  
}