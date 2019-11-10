#' Dimensionality reduction (DR) / Features Reduction
#'
#' This function performs a reduction in the parameters space (the number of variables). It starts by creating a new set of variables, based on a given method (the default method is "PCA", but other are available via the \code{method} argument, such as "cMDS", "DRR" or "ICA"). Then, it names this new dimensions using the original variables that correlates the most with it. For instance, a variable named 'V1_0.97/V4_-0.88' means that the V1 and the V4 variables correlate maximally (with respective coefficients of .97 and -.88) with this dimension. Although this function can be useful in exploratory data analysis, it's best to perform the dimension reduction step in a separate and dedicated stage, as this is a very important process in the data analysis workflow.
#'
#' @inheritParams principal_components
#' @param method The features reduction method. Can be one of 'PCA', 'cMDS', 'DRR', 'ICA' (see the Details section).
#'
#' @details The different methods available are described below:
#'
#' \subsection{Supervised Methods}{
#' \itemize{
#' \item \strong{PCA}: See \code{\link{principal_components}}.
#' }
#'
#' \itemize{
#' \item \strong{cMDS / PCoA}: See \code{\link{cmds}}. Classical Multidimensional Scaling (cMDS) takes a set of dissimilarities (i.e., a distance matrix) and returns a set of points such that the distances between the points are approximately equal to the dissimilarities.
#' }
#'
#' \itemize{
#' \item \strong{DRR}: See \code{\link{DRR}}. Dimensionality Reduction via Regression (DRR) is a very recent technique extending PCA (Laparra et al., 2015). Starting from a rotated PCA, it predicts redundant information from the remaining components using non-linear regression. Some of the most notable advantages of performing PCR are avoidance of multicollinearity between predictors and overfitting mitigation. PCR tends to perform well when the first principal components are enough to explain most of the variation in the predictors. Requires the \pkg{DRR} package to be installed.
#' }
#'
#' \itemize{
#' \item \strong{ICA}: See \code{\link{ICA}}. Performs an Independent Component Analysis using the FastICA algorithm. Contrary to PCA, that attempts to find uncorrelated sources (through least squares minimization), ICA attempts to find independent sources, i.e., the source space that maximizes the "non-gaussianity" of all sources. Contrary to PCA, ICA does not rank each source, which makes it a poor tool for dimensionality reduction. Requires the \pkg{fastICA} package to be installed.
#' }
#'
#'
#' }
#'
#'
#' @references \itemize{
#'  \item Nguyen, L. H., \& Holmes, S. (2019). Ten quick tips for effective dimensionality reduction. PLOS Computational Biology, 15(6).
#'  \item Laparra, V., Malo, J., & Camps-Valls, G. (2015). Dimensionality reduction via regression in hyperspectral imagery. IEEE Journal of Selected Topics in Signal Processing, 9(6), 1026-1036.
#' }
#'
#' @examples
#' parameters_reduction(iris, method = "PCA", n = "max")
#' @importFrom stats dist
#' @export
parameters_reduction <- function(x, method = "PCA", n = "max", ...) {
  UseMethod("parameters_reduction")
}





#' @export
parameters_reduction.data.frame <- function(x, method = "PCA", n = "max", ...) {
  x <- convert_data_to_numeric(x)

  # N factors
  if (n == "max") {
    nfac <- ncol(x) - 1
  } else {
    nfac <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")
  }

  # compute new features
  if (tolower(method) %in% c("pca", "principal")) {
    features <- principal_components(x, n = nfac, ...)
    features <- as.data.frame(attributes(features)$scores)
  } else if (tolower(method) %in% c("cmds", "pcoa")) {
    features <- cmds(x, n = nfac, ...)
  } else if (tolower(method) %in% c("drr")) {
    features <- DRR(x, n = nfac, ...)
  } else if (tolower(method) %in% c("ica")) {
    features <- ICA(x, n = nfac, ...)
  } else {
    stop("'method' must be one of 'PCA', 'cMDS', 'DRR' or 'ICA'.")
  }

  # Get weights / pseudo-loadings (correlations)
  cormat <- as.data.frame(cor(x = x, y = features))
  cormat <- cbind(data.frame(Variable = row.names(cormat)), cormat)
  weights <- as.data.frame(.sort_loadings(cormat, cols = 2:ncol(cormat)))

  if (n == "max") {
    weights <- .filter_loadings(weights, threshold = "max", 2:ncol(weights))
    non_empty <- sapply(weights[2:ncol(weights)], function(x) !all(is.na(x)))
    weights <- weights[c(TRUE, non_empty)]
    features <- features[, non_empty]
    weights[is.na(weights)] <- 0
    weights <- .filter_loadings(.sort_loadings(weights, cols = 2:ncol(weights)), threshold = "max", 2:ncol(weights))
  }

  # Create varnames
  varnames <- sapply(weights[2:ncol(weights)], function(x) {
    name <- weights$Variable[!is.na(x)]
    weight <- insight::format_value(x[!is.na(x)])
    paste0(paste(name, weight, sep = "_"), collapse = "/")
  })
  names(features) <- as.character(varnames)

  # Attributes
  attr(features, "loadings") <- weights
  class(features) <- c("parameters_reduction", class(features))

  # Out
  features
}



#' @export
parameters_reduction.lm <- function(x, method = "PCA", n = "max", ...) {
  data <- parameters_reduction(convert_data_to_numeric(insight::get_predictors(x, ...), ...), method = method, n = n)

  y <- data.frame(.row = 1:length(insight::get_response(x)))
  y[insight::find_response(x)] <- insight::get_response(x)
  y$.row <- NULL

  formula <- paste(insight::find_response(x), "~", paste(paste0("`", names(data), "`"), collapse = " + "))
  update(x, formula = formula, data = cbind(data, y))
}

#' @export
parameters_reduction.merMod <- parameters_reduction.lm





#' @export
principal_components.lm <- function(x, ...) {
  parameters_reduction(x, method = "PCA", ...)
}

#' @export
principal_components.merMod <- principal_components.lm







#' Classical Multidimensional Scaling (cMDS)
#'
#' Also referred to as principal Coordinates Analysis (PCoA), Classical Multidimensional Scaling (cMDS) takes a set of dissimilarities (i.e., a distance matrix) and returns a set of points such that the distances between the points are approximately equal to the dissimilarities.
#'
#' @inheritParams principal_components
#' @param distance The distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given.
#'
#' @references \itemize{
#'  \item Nguyen, L. H., \& Holmes, S. (2019). Ten quick tips for effective dimensionality reduction. PLOS Computational Biology, 15(6).
#' }
#'
#' @examples
#' cmds(iris[, 1:4])
#' @importFrom stats dist
#' @export
cmds <- function(x, n = "all", distance = "euclidean", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  d <- stats::dist(x, method = distance)
  cmd <- stats::cmdscale(d, k = n, eig = TRUE)

  features <- as.data.frame(cmd$points)
  names(features) <- paste0("CMDS", 1:ncol(features))
  features
}









#' Dimensionality Reduction via Regression (DRR)
#'
#' Dimensionality Reduction via Regression (DRR) is a very recent technique extending PCA (Laparra et al., 2015). Starting from a rotated PCA, it predicts redundant information from the remaining components using non-linear regression. Some of the most notable advantages of performing PCR are avoidance of multicollinearity between predictors and overfitting mitigation. PCR tends to perform well when the first principal components are enough to explain most of the variation in the predictors. Requires the \pkg{DRR} package to be installed.
#'
#' @inheritParams principal_components
#'
#' @references \itemize{
#'  \item Laparra, V., Malo, J., & Camps-Valls, G. (2015). Dimensionality reduction via regression in hyperspectral imagery. IEEE Journal of Selected Topics in Signal Processing, 9(6), 1026-1036.
#' }
#'
#' @examples
#' \donttest{
#' DRR(iris[, 1:4])
#' }
#'
#' @importFrom stats dist
#' @export
DRR <- function(x, n = "all", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  if (!requireNamespace("DRR", quietly = TRUE)) {
    stop("Package 'DRR' required for this function to work. Please install it by running `install.packages('DRR')`.")
  }

  junk <- utils::capture.output(suppressMessages(rez <- DRR::drr(x, n)))

  features <- as.data.frame(rez$fitted.data)
  names(features) <- paste0("DRR", 1:ncol(features))
  features
}




#' Independent Component Analysis (ICA)
#'
#' Performs an Independent Component Analysis using the FastICA algorithm. Contrary to PCA, that attempts to find uncorrelated sources (through least squares minimization), ICA attempts to find independent sources, i.e., the source space that maximizes the "non-gaussianity" of all sources. Contrary to PCA, ICA does not rank each source, which makes it a poor tool for dimensionality reduction. Requires the \pkg{fastICA} package to be installed.
#'
#' @inheritParams principal_components
#'
#'
#' @examples
#' ICA(iris[, 1:4])
#' @export
ICA <- function(x, n = "all", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  if (!requireNamespace("fastICA", quietly = TRUE)) {
    stop("Package 'fastICA' required for this function to work. Please install it by running `install.packages('fastICA')`.")
  }

  rez <- fastICA::fastICA(x, n.comp = ncol(x) - 1)

  features <- as.data.frame(rez$S)
  names(features) <- paste0("ICA", 1:ncol(features))
  features
}
