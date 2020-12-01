#' Principal Component Analysis (PCA)
#'
#' This function performs a principal component analysis (PCA) and returns the loadings as a data frame.
#'
#' @param x A data frame or a statistical model.
#' @param n Number of components to extract. If \code{n="all"}, then \code{n} is set as the number of variables minus 1 (\code{ncol(x)-1}). If \code{n="auto"} (default) or \code{n=NULL}, the number of components is selected through \code{\link{n_factors}} resp. \code{\link{n_components}}. In \code{\link{reduce_parameters}}, can also be \code{"max"}, in which case it will select all the components that are maximally pseudo-loaded (i.e., correlated) by at least one variable.
#' @param rotation If not \code{"none"}, the PCA / FA will be computed using the \pkg{psych} package. Possible options include \code{"varimax"}, \code{"quartimax"}, \code{"promax"}, \code{"oblimin"}, \code{"simplimax"}, or \code{"cluster"} (and more). See \code{\link[psych]{fa}} for details.
#' @param sort Sort the loadings.
#' @param threshold A value between 0 and 1 indicates which (absolute) values from the loadings should be removed. An integer higher than 1 indicates the n strongest loadings to retain. Can also be \code{"max"}, in which case it will only display the maximum loading per variable (the most simple structure).
#' @param standardize A logical value indicating whether the variables should be standardized (centered and scaled) to have unit variance before the analysis takes place (in general, such scaling is advisable).
#' @param ... Arguments passed to or from other methods.
#'
#' @details
#'  \subsection{Complexity}{
#'    Complexity represents the number of latent components needed to account
#'    for the observed variables. Whereas a perfect simple structure solution
#'    has a complexity of 1 in that each item would only load on one factor,
#'    a solution with evenly distributed items has a complexity greater than 1
#'    (\cite{Hofman, 1978; Pettersson and Turkheimer, 2010}) .
#'  }
#'  \subsection{Uniqueness}{
#'    Uniqueness represents the variance that is 'unique' to the variable and
#'    not shared with other variables. It is equal to \code{1 – communality}
#'    (variance that is shared with other variables). A uniqueness of \code{0.20}
#'    suggests that 20\% or that variable's variance is not shared with other
#'    variables in the overall factor model. The greater 'uniqueness' the lower
#'    the relevance of the variable in the factor model.
#'  }
#'  \subsection{MSA}{
#'    MSA represents the Kaiser-Meyer-Olkin Measure of Sampling Adequacy
#'    (\cite{Kaiser and Rice, 1974}) for each item. It indicates whether there
#'    is enough data for each factor give reliable results for the PCA. The
#'    value should be > 0.6, and desirable values are > 0.8
#'    (\cite{Tabachnick and Fidell, 2013}).
#'  }
#'  \subsection{PCA or FA?}{
#'  There is a simplified rule of thumb that may help do decide whether to run
#'  a factor analysis or a principal component analysis:
#'  \itemize{
#'    \item Run \emph{factor analysis} if you assume or wish to test a theoretical model of latent factors causing observed variables.
#'    \item Run \emph{principal component analysis} If you want to simply reduce your correlated observed variables to a smaller set of important independent composite variables.
#'  }
#'  (Source: \href{https://stats.stackexchange.com/q/1576/54740}{CrossValidated})
#'  }
#'  \subsection{Computing Item Scores}{
#'    Use \code{\link{get_scores}} to compute scores for the "subscales" represented
#'    by the extracted principal components. \code{get_scores()} takes the results
#'    from \code{principal_components()} and extracts the variables for each
#'    component found by the PCA. Then, for each of these "subscales", row means
#'    are calculated (which equals adding up the single items and dividing by
#'    the number of items). This results in a sum score for each component from
#'    the PCA, which is on the same scale as the original, single items that were
#'    used to compute the PCA.
#'
#'    One can also use \code{predict()} to back-predict scores for each component,
#'    to which one can provide \code{newdata} or a vector of \code{names} for the
#'    components.
#'  }
#'
#' @note There is a \code{summary()}-method that prints the Eigenvalues and (explained) variance for each extracted component. \code{closest_component()} will return a numeric vector with the assigned component index for each column from the original data frame. There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @seealso \code{\link[performance]{check_itemscale}} to compute various measures of internal consistencies applied to the (sub)scales (i.e. components) extracted from the PCA. Use \code{\link{get_scores}} to compute scores for each subscale.
#'
#' @examples
#' library(parameters)
#' if (require("psych")) {
#'   principal_components(mtcars[, 1:7], n = "all", threshold = 0.2)
#'   principal_components(mtcars[, 1:7], n = 2, rotation = "oblimin",
#'                        threshold = "max", sort = TRUE)
#'   principal_components(mtcars[, 1:7], n = 2, threshold = 2, sort = TRUE)
#'
#'   pca <- principal_components(mtcars[, 1:5], n = 2, rotation = "varimax")
#'   pca  # Print loadings
#'   summary(pca)  # Print information about the factors
#'   predict(pca, names=c("Component1", "Component2"))  # Back-predict scores
#'
#'   # which variables from the original data belong to which extracted component?
#'   closest_component(pca)
#'
#' \donttest{
#'   # Automated number of components
#'   principal_components(mtcars[, 1:4], n = "auto")
#' }
#' }
#' @return A data frame of loadings.
#' @references \itemize{
#'   \item Kaiser, H.F. and Rice. J. (1974). Little jiffy, mark iv. Educational and Psychological Measurement, 34(1):111–117
#'   \item Hofmann, R. (1978). Complexity and simplicity as objective indices descriptive of factor solutions. Multivariate Behavioral Research, 13:2, 247-250, \doi{10.1207/s15327906mbr1302_9}
#'   \item Pettersson, E., & Turkheimer, E. (2010). Item selection, evaluation, and simple structure in personality data. Journal of research in personality, 44(4), 407-420, \doi{10.1016/j.jrp.2010.03.002}
#'   \item Tabachnick, B. G., and Fidell, L. S. (2013). Using multivariate statistics (6th ed.). Boston: Pearson Education.
#' }
#' @importFrom stats prcomp
#' @export
principal_components <- function(x, n = "auto", rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {
  UseMethod("principal_components")
}

#' @rdname principal_components
#' @export
closest_component <- function(x) {
  attributes(x)$closest_component
}



#' @importFrom stats prcomp na.omit setNames
#' @export
principal_components.data.frame <- function(x, n = "auto", rotation = "none", sort = FALSE, threshold = NULL, standardize = TRUE, ...) {
  # save name of data set
  data_name <- deparse(substitute(x))

  # original data
  original_data <- x

  # PCA
  model <- stats::prcomp(x, retx = TRUE, center = TRUE, scale. = standardize, ...)


  # N factors
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = rotation)

  # Rotation
  if (rotation != "none") {
    loadings <- .pca_rotate(x, n, rotation = rotation, sort = sort, threshold = threshold, original_data = original_data, ...)
    attr(loadings, "data") <- data_name
    return(loadings)
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
  if (isTRUE(sort)) {
    loadings <- .sort_loadings(loadings)
  }

  # Replace by NA all cells below threshold
  if (!is.null(threshold)) {
    loadings <- .filter_loadings(loadings, threshold = threshold)
  }

  # Add some more attributes
  attr(loadings, "loadings_long") <- .long_loadings(loadings, threshold = threshold)
  # here we match the original columns in the data set with the assigned components
  # for each variable, so we know which column in the original data set belongs
  # to which extracted component...
  attr(loadings, "closest_component") <- .closest_component(loadings, loadings_columns = loading_cols, variable_names = colnames(x))
  attr(loadings, "data") <- data_name
  attr(loadings, "data_set") <- original_data

  # add class-attribute for printing
  class(loadings) <- unique(c("parameters_pca", "see_parameters_pca", class(loadings)))

  loadings
}





#' @keywords internal
.get_n_factors <- function(x, n = NULL, type = "PCA", rotation = "varimax", ...) {
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
.pca_rotate <- function(x, n, rotation, sort = FALSE, threshold = NULL, original_data = NULL, ...) {
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

  pca <- psych::principal(x, nfactors = n, rotate = rotation, ...)
  msa <- psych::KMO(x)

  attr(pca, "MSA") <- msa$MSAi
  out <- model_parameters(pca, sort = sort, threshold = threshold)

  attr(out, "data_set") <- original_data
  out
}



.closest_component <- function(loadings, loadings_columns, variable_names) {
  component_columns <- apply(loadings[loadings_columns], 1, function(i) which.max(abs(i)))
  stats::setNames(component_columns, variable_names)
}