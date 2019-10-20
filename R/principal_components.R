#' Principal Component Analysis (PCA)
#'
#' This function performs a principal component analysis (PCA) and returns the loadings as a dataframe.
#'
#' @param x A dataframe or a statistical model.
#' @param n Number of components to extract. If \code{n = "all"} =, then \code{n} is set as the number of variables minus 1 (\code{ncol(x)-1}). If \code{n = "auto"} (default) or \code{n = NULL}, the number of components is selected through \code{\link{n_factors}}. In \code{\link{parameters_reduction}}, can also be \code{"max"}, in which case it will select all the components that are maximally pseudo-loaded (i.e., correlated) by at least one variable.
#' @param rotation If not "none", the PCA will be computed using the \pkg{psych} package. Possible options include \code{"varimax"}, \code{"quartimax"}, \code{"promax"}, \code{"oblimin"}, \code{"simplimax"}, and \code{"cluster"}. See \code{\link[psych]{fa}} for details.
#' @param sort Sort the loadings.
#' @param threshold A value between 0 and 1 indicates which (absolute) values from the loadings should be removed. An integer higher than 1 indicates the n strongest loadings to retain. Can also be "max", in which case it will only display the maximum loading per variable (the most simple structure).
#' @param standardize A logical value indicating whether the variables should be standardized (centred and scaled) to have unit variance before the analysis takes place (in general, such scaling is advisable).
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \itemize{
#'    \item \strong{Complexity} (Hoffman's, 1978; Pettersson and Turkheimer, 2010) represents the number of latent components needed to account for the observed variables. Whereas a perfect simple structure solution has a complexity of 1 in that each item would only load on one factor, a solution with evenly distributed items has a complexity greater than 1.
#' }
#'
#' @note There is a \code{summary()}-method that prints the Eigenvalues and (explained) variance for each extracted component.
#'
#' @examples
#' library(parameters)
#'
#' principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#' principal_components(mtcars[, 1:7], n = 2, rotation = "oblimin", threshold = "max", sort = TRUE)
#' principal_components(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#'
#' pca <- principal_components(mtcars[, 1:5], n = 2)
#' summary(pca)
#' predict(pca)
#' \donttest{
#' # Automated number of components
#' principal_components(mtcars[, 1:4], n = "auto")
#' }
#'
#' @return A data.frame of loadings.
#' @references \itemize{
#'   \item Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420.
#' }
#' @importFrom stats prcomp
#' @export
principal_components <- function(x, n = "auto", rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {
  UseMethod("principal_components")
}



#' @importFrom stats prcomp na.omit
#' @export
principal_components.data.frame <- function(x, n = "auto", rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {

  # Standardize
  if (standardize) {
    x <- as.data.frame(scale(x))
  }

  # PCA
  model <- stats::prcomp(x, retx = TRUE, center = TRUE, scale. = TRUE, ...)


  # N factors
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = rotation)

  # Rotation
  if (rotation != "none") {
    return(.pca_rotate(x, n, rotation = rotation, sort = sort, threshold = threshold, ...))
  }

  # Re-add centers and scales
  if (standardize) {
    model$center <- attributes(x)$center
    model$scale <- attributes(x)$scale
  }

  # Summary (cumulative variance etc.)
  eigenvalues <- model$sdev^2
  data_summary <- .data_frame(
    Component = sprintf("PC%i", seq_len(length(model$sdev))),
    Eigenvalues = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues),
    Variance_Cumulative = cumsum(eigenvalues / sum(eigenvalues))
  )
  data_summary$Variance_Proportion <- data_summary$Variance / sum(data_summary$Variance)

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
  attr(loadings, "standardize") <- standardize
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
    loadings <- .filter_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold)

  # add class-attribute for printing
  class(loadings) <- unique(c("parameters_pca", "see_parameters_pca", class(loadings)))

  loadings
}





#' @keywords internal
.get_n_factors <- function(x, n = NULL, type = "PCA", rotation = "PCA", ...) {
  # N factors
  if (is.null(n) || n == "auto") {
    n <- as.numeric(n_factors(x, type = type, rotation = rotation, ...))
  } else if (n == "all") {
    n <- ncol(x) - 1
  } else if (n >= ncol(x)) {
    n <- ncol(x) - 1
  }
  n
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
