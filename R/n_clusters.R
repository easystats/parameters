#' Find how many clusters in your data
#'
#' Similarly to [n_factors()] for factor / principal component analysis,
#' \code{n_clusters} is the main function to find out the optimal numbers of clusters
#' present in the data based on the maximum consensus of a large number of methods.
#' \cr
#' Essentially, there exist many methods to determine the optimal number of clusters,
#' each with pros and cons, benefits and limitations. The main \code{n_clusters}
#' function proposes to run all of them, and find out the number of clusters that is
#' suggested by the majority of methods (in case of ties, it will select the
#' most parsimonious solution with fewer clusters).
#' \cr
#' Note that we also implement some specific, commonly used methods, like the
#' Elbow or the Gap method, with their own visualisation functionalities. See
#' the examples below for more details.
#'
#' @inheritParams check_clusterstructure
#' @param include_factors Logical, if `TRUE`, factors are converted to numerical
#'   values in order to be included in the data for determining the number of
#'   clusters. By default, factors are removed, because most methods that
#'   determine the number of clusters need numeric input only.
#' @param package Package from which methods are to be called to determine the
#'   number of clusters. Can be `"all"` or a vector containing
#'   `"easystats"`, `"NbClust"`, `"mclust"`, `"cluster"` and `"M3C"`.
#' @param fast If `FALSE`, will compute 4 more indices (sets `index =
#'   "allong"` in `NbClust`). This has been deactivated by default as it is
#'   computationally heavy.
#' @param n_max Maximal number of clusters to test.
#' @param clustering_function,gap_method Other arguments passed to other functions. \code{clustering_function} is used by \code{fviz_nbclust} and can be \code{kmeans}, code{cluster::pam}, code{cluster::clara}, code{cluster::fanny}, and more. \code{gap_method} is used by \code{cluster::maxSE} to extract the optimal numbers of clusters (see its \code{method} argument).
#' @param method,min_size,eps_length,eps_range Arguments for DBSCAN algorithm.
#' @param distance_method The distance method (passed to [dist()]). Used by algorithms relying on the distance matrix, such as \code{hclust} or \code{dbscan}.
#' @param hclust_method The hierarchical clustering method (passed to [hclust()]).
#' @inheritParams model_parameters.glm
#'
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(parameters)
#' \donttest{
#' # The main 'n_clusters' function ===============================
#' if (require("mclust", quietly = TRUE) && require("NbClust", quietly = TRUE) && require("see")) {
#'   n <- n_clusters(iris[, 1:4], package = c("easystats", "NbClust", "mclust"))
#'   n
#'   summary(n)
#'   as.data.frame(n)
#'   plot(n)
#'
#'   # The following runs all the method but it significantly slower
#'   # n_clusters(iris[, 1:4], standardize = FALSE, package = "all", fast = FALSE)
#' }
#' }
#' @export
n_clusters <- function(x,
                       standardize = TRUE,
                       include_factors = FALSE,
                       package = c("easystats", "NbClust", "mclust", "M3C"),
                       fast = TRUE,
                       ...) {
  if (all(package == "all")) {
    package <- c("easystats", "NbClust", "mclust", "M3C")
  }

  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  out <- data.frame()

  if ("easystats" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_easystats(x, ...))
  }

  if ("nbclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_NbClust(x, fast = fast))
  }

  if ("mclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_mclust(x))
  }

  if ("M3C" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_M3C(x, fast = fast))
  }


  out <- out[order(out$n_Clusters), ] # Arrange by n clusters
  row.names(out) <- NULL # Reset row index
  out$Method <- as.character(out$Method)

  # Remove duplicate methods starting with the smallest
  dupli <- c()
  for (i in 1:nrow(out)) {
    if (i > 1 && out[i, "Method"] %in% out$Method[1:i - 1]) {
      dupli <- c(dupli, i)
    }
  }

  if (!is.null(dupli)) {
    out <- out[-dupli, ]
  }

  # Add summary
  by_clusters <- .data_frame(
    n_Clusters = as.numeric(unique(out$n_Clusters)),
    n_Methods = as.numeric(by(out, as.factor(out$n_Clusters), function(out) n <- nrow(out)))
  )

  attr(out, "summary") <- by_clusters
  attr(out, "n") <- min(as.numeric(as.character(by_clusters[by_clusters$n_Methods == max(by_clusters$n_Methods), c("n_Clusters")])))

  class(out) <- c("n_clusters", "see_n_clusters", class(out))
  out
}




#' @keywords internal
.n_clusters_mclust <- function(x, ...) {
  insight::check_if_installed("mclust")
  mclustBIC <- mclust::mclustBIC # this is needed as it is internally required by the following function
  BIC <- mclust::mclustBIC(x, verbose = FALSE)
  out <- data.frame(unclass(BIC))
  n <- which(out == max(out, na.rm = TRUE), arr.ind = TRUE)[1]
  data.frame(n_Clusters = n, Method = "Mixture", Package = "mclust")
}




# Methods -----------------------------------------------------------------


#' @keywords internal
.n_clusters_easystats <- function(x, ...) {
  elb <- n_clusters_elbow(x, preprocess = FALSE, ...)
  sil <- n_clusters_silhouette(x, preprocess = FALSE, ...)
  gap1 <- n_clusters_gap(x, preprocess = FALSE, gap_method = "firstSEmax", ...)
  gap2 <- n_clusters_gap(x, preprocess = FALSE, gap_method = "globalSEmax", ...)

  data.frame(n_Clusters = c(attributes(elb)$n, attributes(sil)$n, attributes(gap1)$n, attributes(gap2)$n),
             Method = c("Elbow", "Silhouette", "Gap_Maechler2012", "Gap_Dudoit2002"),
             Package = "easystats")
}




#' @keywords internal
.n_clusters_NbClust <- function(x, fast = TRUE, ...) {
  insight::check_if_installed("NbClust")

  # Run the function and suppress output and automatic plotting
  ff <- tempfile()
  grDevices::png(filename = ff)
  if (fast) {
    indices <- "all"
  } else {
    indices <- "alllong"  # This is not misspelling (it's like that in the function)
  }

  junk <- utils::capture.output(n <- NbClust::NbClust(
    x,
    index = indices,
    method = "ward.D2"
  ))
  grDevices::dev.off()
  unlink(ff)

  out <- as.data.frame(t(n$Best.nc))
  data.frame(n_Clusters = out$Number_clusters, Method = row.names(out), Package = "NbClust")
}


#' @keywords internal
.n_clusters_M3C <- function(x, fast = TRUE, ...) {
  if (!requireNamespace("M3C", quietly = TRUE)) {
    stop("Package 'M3C' required for this function to work. Please install it from Bioconductor by first running `install.packages(\"BiocManager\")`, then `BiocManager::install(\"M3C\")`.") # Not on CRAN (but on github and bioconductor)
  }

  data <- data.frame(t(x))
  colnames(data) <- paste0("x", seq(1, ncol(data))) # Add columns names as required by the package

  suppressMessages(out <- M3C::M3C(data, method = 2))

  out <- data.frame(
    n_Clusters = which.max(out$scores$PCSI),
    Method = "Consensus clustering algorithm (penalty term)",
    Package = "M3C"
  )

  # Doesn't work
  # if (fast == FALSE){
  #   suppressMessages(out <- M3C::M3C(data, method=1))
  #   out <- rbind(out, data.frame(n_Clusters = which.max(out$scores$RCSI), Method = "Consensus clustering algorithm (Monte Carlo)", Package = "M3C"))
  # }

  out
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
