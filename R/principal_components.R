#' Principal Component Analysis (PCA)
#'
#' This function performs a principal component analysis (PCA) and returns the loadings (of the unrotated matrix) as dataframe.
#'
#' @param x A dataframe.
#' @param n Number of components to extract. If \code{n = NULL}, the number of components is selected through \code{\link{n_factors}}.
#' @param rotation If not "none", the PCA will be computed using the \code{psych} package. Possible options include "varimax", "quartimax", "promax", "oblimin", "simplimax", and "cluster". See \code{psych::fa} for details.
#' @param sort Sort the loadings.
#' @param threshold A value between 0 and 1 indicates which (absolute) values from the loadings should be removed. Can also be "max", in which case it will only display the maximum loading per veriable (the most simple structure).
#' @param standardize A logical value indicating whether the variables should be standardized (centred and scaled) to have unit variance before the analysis takes place (in general, such scaling is advisable).
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \itemize{
#'    \item \strong{Complexity} (Hoffman's, 1978; Pettersson and Turkheimer, 2010) represents the number of latent components needed to account for the observed variables. Whereas a perfect simple structure solution has a complexity of 1 in that each item would only load on one factor, a solution with evenly distributed items has a complexity greater than 1.
#' }
#'
#'
#' @examples
#' library(parameters)
#'
#' principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#' principal_components(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
#'
#' pca <- principal_components(mtcars[, 1:5], n = 2)
#' summary(pca)
#' predict(pca)
#'
#' \donttest{
#' # Automated number of components
#' principal_components(mtcars[, 1:4])
#' }
#'
#' @return A data.frame of loadings.
#' @references \itemize{
#'   \item Pettersson, E., \& Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420.
#' }
#' @importFrom stats prcomp
#' @export
principal_components <- function(x, n = NULL, rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {
  UseMethod("principal_components")
}

#' @rdname principal_components
#' @export
PCA <- principal_components


#' @importFrom stats prcomp na.omit
#' @export
principal_components.data.frame <- function(x, n = NULL, rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {

  # Standardize
  if (standardize) {
    x <- standardize(x, ...)
  }

  if(rotation != "none"){
    return(.pca_rotate(x, n, rotation = rotation, sort = sort, threshold = threshold, ...))
  }

  # PCA
  model <- stats::prcomp(x, retx = TRUE, center = TRUE, scale. = TRUE, ...)


  # N factors
  if (is.null(n)) {
    n <- as.numeric(n_factors(x, type = "PCA", rotation = "none", ...))
  } else if (n == "all") {
    n <- length(model$sdev)
  } else if (n > length(model$sdev)) {
    n <- length(model$sdev)
  }

  # Re-add centers and scales
  if (standardize) {
    model$center <- attributes(x)$center
    model$scale <- attributes(x)$scale
  }

  # Summary (cumulative variance etc.)
  eigenvalues <- model$sdev^2
  data_summary <- data_frame(
    Component = sprintf("PC%i", seq_len(length(model$sdev))),
    Eigenvalues = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues),
    Variance_Cumulative = cumsum(eigenvalues / sum(eigenvalues))
  )

  model$sdev <- model$sdev[1:n]
  model$rotation <- model$rotation[, 1:n, drop = FALSE]
  model$x <- model$x[, 1:n, drop = FALSE]
  data_summary <- data_summary[1:n, , drop = FALSE]



  # Compute loadings
  if (length(model$sdev) > 1) {
    loadings <- as.data.frame(model$rotation %*% diag(model$sdev))
  } else {
    loadings <- as.data.frame(model$rotation %*% model$sdev)
  }
  names(loadings) <- data_summary$Component


  # Format
  loadings <- cbind(data.frame(Variable = row.names(loadings)), loadings)
  row.names(loadings) <- NULL

  # Add information
  loading_cols <- 2:(n + 1)
  loadings$Complexity <- (apply(loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^2)))^2 / apply(loadings[, loading_cols, drop = FALSE], 1, function(x) sum(x^4))

  # Add attributes
  attr(loadings, "summary") <- data_summary
  attr(loadings, "model") <- model
  attr(loadings, "rotation") <- "none"
  attr(loadings, "scores") <- model$x
  # attr(loadings, "standardize") <- standardize
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- n
  attr(loadings, "type") <- "prcomp"
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











#' @keywords internal
.pca_rotate <- function(x, n, rotation, sort = FALSE, threshold = NULL, ...) {
  if (!(rotation %in% c("varimax", "quartimax", "promax", "oblimin", "simplimax", "cluster", "none"))) {
    stop("`rotation` must be one of \"varimax\", \"quartimax\", \"promax\", \"oblimin\", \"simplimax\", \"cluster\" or \"none\".")
  }

  if (!inherits(x, c("prcomp", "data.frame"))) {
    stop("`x` must be of class `prcomp` or a data frame.", call. = FALSE)
  }

  if (!inherits(x, "data.frame") && rotation != "varimax") {
    stop(sprintf("`x` must be a data frame for `%s`-rotation.", rotation), call. = FALSE)
  }

  # rotate loadings
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop(sprintf("Package `psych` required for `%s`-rotation.", rotation), call. = FALSE)
  }

  model_parameters(psych::principal(x, nfactors = n, rotate = rotation, ...), sort = sort, threshold = threshold)
}
