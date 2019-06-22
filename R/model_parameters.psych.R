#' Format PCA/FA from the psych package
#'
#' Format PCA/FA objects from the psych package (Revelle, 2016).
#'
#' @param model PCA or FA created by the \code{psych::principal} or \code{psych::fa} functions.
#' @inheritParams principal_components
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \itemize{
#'    \item \strong{Complexity} (Hoffman's, 1978; Pettersson and Turkheimer, 2010) represents the number of latent components needed to account for the observed variables. Whereas a perfect simple structure solution has a complexity of 1 in that each item would only load on one factor, a solution with evenly distributed items has a complexity greater than 1.
#'    \item \strong{Uniqueness} represents the variance that is 'unique' to the variable and not shared with other variables. It is equal to \code{1 – communality} (variance that is shared with other variables). A uniqueness of \code{0.20} suggests that 20\% or that variable is not shared with other variables in the overall factor model. The greater 'uniqueness' the lower the relevance of the variable in the factor model.
#' }
#'
#' @examples
#' library(parameters)
#' \dontrun{
#' library(psych)
#'
#' pca <- psych::principal(attitude)
#' model_parameters(pca)
#'
#' pca <- psych::principal(attitude, nfactors = 3, rotate="none")
#' model_parameters(pca, sort = TRUE, threshold = 0.2)
#' }
#' # Note that the latter is identical to the 'principal_components' function available in parameters:
#' principal_components(attitude, n = 3, sort = TRUE, threshold = 0.2)
#'
#' @references \itemize{
#'   \item Pettersson, E., \& Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420.
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis and data reduction.
#' }
#' @export
model_parameters.principal <- function(model, sort = FALSE, threshold = NULL, ...) {

  # PCA
  pca <- model

  n <- model$factors

  # Get summary
  variance <- as.data.frame(unclass(model$Vaccounted))
  data_summary <- data_frame(
    Component = names(variance),
    Eigenvalues = model$values[1:n],
    Variance = as.numeric(variance["Proportion Var", ])
  )
  if("Cumulative Var" %in% row.names(variance)){
    data_summary$Variance_Cumulative <- as.numeric(variance["Cumulative Var", ])
  } else{
    if(ncol(variance) == 1){
      data_summary$Variance_Cumulative <- as.numeric(variance["Proportion Var", ])
    } else{
      data_summary$Variance_Cumulative <- NA
    }
  }

  # Get loadings
  loadings <- as.data.frame(unclass(model$loadings))

  # Best representation (max loading)
  rowmax_index <- sapply(as.data.frame(t(loadings)), function(x) which.max(abs(x)))
  rowmax <- sapply(as.data.frame(t(loadings)), function(x) x[which.max(abs(x))])
  loadings_max <- data_frame(Component = names(loadings)[rowmax_index], Loading = rowmax)

  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL
  loadings_max <- cbind(data.frame(Variable = row.names(loadings_max)), loadings_max)
  row.names(loadings_max) <- NULL

  # Add information
  loading_cols <- 2:(n+1)
  loadings$Complexity <- model$complexity
  loadings$Uniqueness <- model$uniquenesses

  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "pca") <- pca
  attr(loadings, "rotation") <- pca$rotation
  attr(loadings, "scores") <- pca$scores
  attr(loadings, "loadings_max") <- loadings_max
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n


  # Sorting
  if (sort) {
    loadings <- .sort_loadings(loadings, cols = loading_cols)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filer_loadings(loadings, cols = loading_cols, threshold = threshold)
  }

  # add class-attribute for printing
  class(loadings) <- c("PCA", class(loadings))

  loadings

}



# #' @export
# principal.lm <- function(x, ...) {
#   if (!requireNamespace("psych", quietly = TRUE)) {
#     stop("The package 'psych' is needed. Please install it by running 'install.packages(psych)'.")
#   }
#   psych::principal(insight::get_predictors(x, ...), ...)
# }
