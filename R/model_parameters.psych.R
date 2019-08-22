#' Format PCA/FA from the psych package
#'
#' Format PCA/FA objects from the psych package (Revelle, 2016).
#'
#' @param model PCA or FA created by the \code{psych::principal} or \code{psych::fa} functions.
#' @inheritParams principal_components
#' @param labels A character vector containing labels to be added to the loadings data. Usually, the question related to the item.
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \itemize{
#'    \item \strong{Complexity} (Hoffman's, 1978; Pettersson and Turkheimer, 2010) represents the number of latent components needed to account for the observed variables. Whereas a perfect simple structure solution has a complexity of 1 in that each item would only load on one factor, a solution with evenly distributed items has a complexity greater than 1.
#'    \item \strong{Uniqueness} represents the variance that is 'unique' to the variable and not shared with other variables. It is equal to \code{1 – communality} (variance that is shared with other variables). A uniqueness of \code{0.20} suggests that 20\% or that variable's variance is not shared with other variables in the overall factor model. The greater 'uniqueness' the lower the relevance of the variable in the factor model.
#' }
#'
#' @examples
#' library(parameters)
#' library(psych)
#'
#' \donttest{
#' # Principal Component Analysis (PCA) ---------
#' pca <- psych::principal(attitude)
#' model_parameters(pca)
#' }
#'
#' pca <- psych::principal(attitude, nfactors = 3, rotate = "none")
#' model_parameters(pca, sort = TRUE, threshold = 0.2)
#'
#' principal_components(attitude, n = 3, sort = TRUE, threshold = 0.2)
#' \donttest{
#' # Exploratory Factor Analysis (EFA) ---------
#' efa <- psych::fa(attitude, nfactors = 3)
#' model_parameters(efa, threshold = "max", sort = TRUE, labels = as.character(1:ncol(attitude)))
#' }
#'
#' @return A data.frame of loadings.
#' @references \itemize{
#'   \item Pettersson, E., \& Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420.
#'   \item Revelle, W. (2016). How To: Use the psych package for Factor Analysis and data reduction.
#' }
#' @export
model_parameters.principal <- function(model, sort = FALSE, threshold = NULL, labels = NULL, ...) {

  # PCA
  n <- model$factors

  # Get summary
  variance <- as.data.frame(unclass(model$Vaccounted))
  data_summary <- data_frame(
    Component = names(variance),
    Eigenvalues = model$values[1:n],
    Variance = as.numeric(variance["Proportion Var", ])
  )
  if ("Cumulative Var" %in% row.names(variance)) {
    data_summary$Variance_Cumulative <- as.numeric(variance["Cumulative Var", ])
  } else {
    if (ncol(variance) == 1) {
      data_summary$Variance_Cumulative <- as.numeric(variance["Proportion Var", ])
    } else {
      data_summary$Variance_Cumulative <- NA
    }
  }

  # Get loadings
  loadings <- as.data.frame(unclass(model$loadings))

  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL

  # Labels
  if(!is.null(labels)){
    loadings$Label <- labels
    loadings <- loadings[c("Variable", "Label", names(loadings)[!names(loadings) %in% c("Variable", "Label")])]
    loading_cols <- 3:(n + 2)
  } else{
    loading_cols <- 2:(n + 1)
  }

  # Add information
  loadings$Complexity <- model$complexity
  loadings$Uniqueness <- model$uniquenesses

  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- model$rotation
  attr(loadings, "scores") <- model$scores
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n
  attr(loadings, "type") <- model$fn
  attr(loadings, "loadings_columns") <- loading_cols

  # Sorting
  if (sort) {
    loadings <- .sort_loadings(loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filer_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold)

  # add class-attribute for printing
  class(loadings) <- c("parameters_efa", class(loadings))

  loadings
}




#' @export
model_parameters.fa <- model_parameters.principal
