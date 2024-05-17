#' Chelsea Song's Pareto-optimization Procedure
#'
#' @param data Dataframe generated via AssessmentGen methods.
#' @param predictors Character vector containing strings corresponding to predictors of interest
#' @param sr A priori selection ratio.
#' @param d Numeric vector of subgroup difference magnitudes. If no vector is provided, 
#' B-W differences from package will be used. 
#' @param corrMat Correlation matrix containing relationships between each predictor. 
#' If no matrix is provided, correlation matrix from package will be used.
#' @param Spac Number of solutions devised via Pareto-optimization.
#' @param graph Logical; should graph be shown?
#' @param display_solution Logical; should solution be shown?
#'
#' @return Pareto-optimization graphs and solutions. 
#' @export 
#'
#' @examples data <- auto_generation(n = 1000, seed = 777)
#'           songPareto(data, c("Structured Interview", "GMA tests", "Conscientiousness"), sr = .20)
#' @import ParetoR
#' 
songPareto <- function(data, predictors, sr, d = NULL, corrMat = NULL, Spac = 20, graph = T, 
                        display_solution = T) { 
  
  prop <- min(c((sum(data$Race == "Black") / sum(data$Race == "White")), 
                sum(data$Race == "White") / sum(data$Race == "Black")))
  
  if(is.null(d)) {
    path_bwai <- system.file("extdata", "black_white.xlsx", package = "AssessmentGen")
    d <- read_excel(path_bwai)
  }
  
  if (!all(predictors %in% colnames(d))) {
    stop("One or more predictors are not in the difference vector")
  }
  
  d <- as.vector(d[, predictors, drop = FALSE])
  d <- unlist(d)
  d <- unname(d)
  
  if (!all(predictors %in% colnames(data))) {
    stop("One or more predictors are not in the data frame columns")
  }
  
  data <- data[, predictors, drop = FALSE]
  
  if(is.null(corrMat)) {
    predictors <- append(predictors, "Job Performance")
    path_corr <- system.file("extdata", "Correlation.xlsx", package = "AssessmentGen")
    corrMat <- as.data.frame(read_excel(path_corr))
    rownames(corrMat) <- colnames(corrMat)
  }
  
  if (!all(predictors %in% colnames(corrMat))) {
    stop("One or more predictors are not in the correlation matrix columns")
  }
  
  corrMat <- as.matrix(corrMat[predictors, predictors, drop = FALSE])
    
  ParetoR(prop, sr, d, R = corrMat, Spac, graph, display_solution)
}