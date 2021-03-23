#' Dimensionality reduction (DR) / Features Reduction
#'
#' This function performs a reduction in the parameter space (the number of
#' variables). It starts by creating a new set of variables, based on the given
#' method (the default method is "PCA", but other are available via the
#' \code{method} argument, such as "cMDS", "DRR" or "ICA"). Then, it names this
#' new dimensions using the original variables that correlates the most with it.
#' For instance, a variable named 'V1_0.97/V4_-0.88' means that the V1 and the
#' V4 variables correlate maximally (with respective coefficients of .97 and
#' -.88) with this dimension. Although this function can be useful in
#' exploratory data analysis, it's best to perform the dimension reduction step
#' in a separate and dedicated stage, as this is a very important process in the
#' data analysis workflow. \code{reduce_data()} is an alias for
#' \code{reduce_parameters.data.frame()}.
#'
#' @inheritParams principal_components
#' @param method The feature reduction method. Can be one of 'PCA', 'cMDS',
#'   'DRR', 'ICA' (see the Details section).
#' @param distance The distance measure to be used. Only applies when
#'   \code{method = "cMDS"}. This must be one of "euclidean", "maximum",
#'   "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring
#'   can be given.
#'
#' @details The different methods available are described below:
#' \subsection{Supervised Methods}{
#' \itemize{
#' \item \strong{PCA}: See \code{\link{principal_components}}.
#'
#' \item \strong{cMDS / PCoA}: Classical Multidimensional Scaling (cMDS) takes a
#' set of dissimilarities (i.e., a distance matrix) and returns a set of points
#' such that the distances between the points are approximately equal to the
#' dissimilarities.
#'
#' \item \strong{DRR}: Dimensionality Reduction via Regression (DRR) is a very
#' recent technique extending PCA (Laparra et al., 2015). Starting from a
#' rotated PCA, it predicts redundant information from the remaining components
#' using non-linear regression. Some of the most notable advantages of
#' performing DRR are avoidance of multicollinearity between predictors and
#' overfitting mitigation. DRR tends to perform well when the first principal
#' component is enough to explain most of the variation in the predictors.
#' Requires the \pkg{DRR} package to be installed.
#'
#' \item \strong{ICA}: Performs an Independent Component Analysis using the
#' FastICA algorithm. Contrary to PCA, which attempts to find uncorrelated
#' sources (through least squares minimization), ICA attempts to find
#' independent sources, i.e., the source space that maximizes the
#' "non-gaussianity" of all sources. Contrary to PCA, ICA does not rank each
#' source, which makes it a poor tool for dimensionality reduction. Requires the
#' \pkg{fastICA} package to be installed.
#' }
#' }
#' See also \href{https://CRAN.R-project.org/package=parameters/vignettes/parameters_reduction.html}{package vignette}.
#'
#' @references \itemize{
#'  \item Nguyen, L. H., \& Holmes, S. (2019). Ten quick tips for effective
#'  dimensionality reduction. PLOS Computational Biology, 15(6).
#'  \item Laparra, V., Malo, J., & Camps-Valls, G. (2015). Dimensionality
#'  reduction via regression in hyperspectral imagery. IEEE Journal of Selected
#'  Topics in Signal Processing, 9(6), 1026-1036.
#' }
#'
#' @examples
#' data(iris)
#' model <- lm(Sepal.Width ~ Species * Sepal.Length + Petal.Width, data = iris)
#' model
#' reduce_parameters(model)
#'
#' out <- reduce_data(iris, method = "PCA", n = "max")
#' head(out)
#' @importFrom stats dist cor cmdscale update
#' @export
reduce_parameters <- function(x, method = "PCA", n = "max", distance = "euclidean", ...) {
  UseMethod("reduce_parameters")
}


#' @rdname reduce_parameters
#' @export
reduce_data <- function(x, method = "PCA", n = "max", distance = "euclidean", ...) {
  if (!is.data.frame(x)) {
    stop("Only works on data frames.")
  }
  reduce_parameters(x, method = method, n = n, distance = distance, ...)
}



#' @export
reduce_parameters.data.frame <- function(x, method = "PCA", n = "max", distance = "euclidean", ...) {
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
    features <- .cmds(x, n = nfac, distance = distance, ...)
  } else if (tolower(method) %in% c("drr")) {
    features <- .drr(x, n = nfac, ...)
  } else if (tolower(method) %in% c("ica")) {
    features <- .ica(x, n = nfac, ...)
  } else {
    stop("'method' must be one of 'PCA', 'cMDS', 'DRR' or 'ICA'.")
  }

  # Get weights / pseudo-loadings (correlations)
  cormat <- as.data.frame(stats::cor(x = x, y = features))
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
reduce_parameters.lm <- function(x, method = "PCA", n = "max", distance = "euclidean", ...) {
  data <- reduce_parameters(convert_data_to_numeric(insight::get_predictors(x, ...), ...), method = method, n = n, distance = distance)

  y <- data.frame(.row = 1:length(insight::get_response(x)))
  y[insight::find_response(x)] <- insight::get_response(x)
  y$.row <- NULL

  formula <- paste(insight::find_response(x), "~", paste(paste0("`", names(data), "`"), collapse = " + "))
  stats::update(x, formula = formula, data = cbind(data, y))
}

#' @export
reduce_parameters.merMod <- reduce_parameters.lm





#' @export
principal_components.lm <- function(x, ...) {
  reduce_parameters(x, method = "PCA", ...)
}

#' @export
principal_components.merMod <- principal_components.lm







#' @keywords internal
.cmds <- function(x, n = "all", distance = "euclidean", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  d <- stats::dist(x, method = distance)
  cmd <- stats::cmdscale(d, k = n, eig = TRUE)

  features <- as.data.frame(cmd$points)
  names(features) <- paste0("CMDS", 1:ncol(features))
  features
}








#' @importFrom utils capture.output
#' @keywords internal
.drr <- function(x, n = "all", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  if (!requireNamespace("DRR", quietly = TRUE)) {
    stop("Package 'DRR' required for this function to work. Please install it by running `install.packages('DRR')`.")
  }

  junk <- utils::capture.output(suppressMessages(rez <- DRR::drr(x, n)))

  features <- as.data.frame(rez$fitted.data)
  names(features) <- paste0("DRR", 1:ncol(features))
  features
}




#' @keywords internal
.ica <- function(x, n = "all", ...) {
  n <- .get_n_factors(x, n = n, type = "PCA", rotation = "none")

  if (!requireNamespace("fastICA", quietly = TRUE)) {
    stop("Package 'fastICA' required for this function to work. Please install it by running `install.packages('fastICA')`.")
  }

  rez <- fastICA::fastICA(x, n.comp = ncol(x) - 1)

  features <- as.data.frame(rez$S)
  names(features) <- paste0("ICA", 1:ncol(features))
  features
}
