#' Find the cluster centers in your data
#'
#' For each cluster, computes the mean (or other indices) of the variables. Can be used
#' to retrieve the centrers of clusters.
#'
#' @param data A data.frame.
#' @param clusters A vector with clusters assignments (must be same length as rows in data).
#' @param fun What function to use, \code{mean} by default.
#'
#' @examples
#' k <- kmeans(iris[1:4], 3)
#' cluster_centers(iris[1:4], k$cluster)
#' cluster_centers(iris[1:4], k$cluster, median)
#' @export
cluster_centers <- function(data, clusters, fun = mean) {
  aggregate(data, list(Cluster=clusters), fun)
}





# Utils -------------------------------------------------------------------

#' @keywords internal
.prepare_data_clustering <- function(x, include_factors = FALSE, standardize = FALSE, preprocess = TRUE, ...) {
  if(preprocess == FALSE) return(x)

  # Convert factors to numeric
  # include factors?
  if (include_factors) {
    # ordered factors to numeric
    factors <- sapply(x, is.ordered)
    if (any(factors)) {
      x[factors] <- sapply(x[factors], .factor_to_numeric)
    }

    # character and factors to dummies
    factors <- sapply(x, function(i) is.character(i) | is.factor(i))
    if (any(factors)) {
      dummies <- lapply(x[factors], .factor_to_dummy)
      x <- cbind(x[!factors], dummies)
    }
  } else {
    # remove factors
    x <- x[sapply(x, is.numeric)]
  }

  # Remove all missing values from data, only use numerics
  x <- stats::na.omit(x)
  if (standardize == TRUE) {
    x <- datawizard::standardize(x, ...)
  }
  x
}
