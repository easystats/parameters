#' @title Compute cluster analysis and return group indices
#' @name cluster_analysis
#' @description Compute hierarchical or kmeans cluster analysis and return the group
#'                assignment for each observation as vector.
#'
#' @references Maechler M, Rousseeuw P, Struyf A, Hubert M, Hornik K (2014) cluster: Cluster Analysis Basics and Extensions. R package.
#'
#' @param x A data frame.
#' @param n_clusters Number of clusters used for the cluster solution. By default,
#'   the number of clusters to extract is determined by calling \code{\link{n_clusters}}.
#' @param method Method for computing the cluster analysis. By default (\code{"hclust"}), a
#'   hierarchical cluster analysis, will be computed. Use \code{"kmeans"} to
#'   compute a kmeans cluster analysis. You can specify the initial letters only.
#' @param distance Distance measure to be used when \code{method = "hclust"} (for hierarchical
#'   clustering). Must be one of \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
#'   \code{"canberra"}, \code{"binary"} or \code{"minkowski"}. See \code{\link{dist}}.
#'   If is \code{method = "kmeans"} this argument will be ignored.
#' @param agglomeration Agglomeration method to be used when \code{method = "hclust"} (for hierarchical
#'   clustering). This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"},
#'   \code{"mcquitty"}, \code{"median"} or \code{"centroid"}. Default is \code{"ward"} (see \code{\link{hclust}}).
#'   If \code{method = "kmeans"} this argument will be ignored.
#' @param iterations Maximum number of iterations allowed. Only applies, if
#'   \code{method = "kmeans"}. See \code{\link{kmeans}} for details on this argument.
#' @param algorithm Algorithm used for calculating kmeans cluster. Only applies, if
#'   \code{method = "kmeans"}. May be one of \code{"Hartigan-Wong"} (default),
#'   \code{"Lloyd"} (used by SPSS), or \code{"MacQueen"}. See \code{\link{kmeans}}
#'   for details on this argument.
#' @param force Logical, if \code{TRUE}, ordered factors (ordinal variables) are
#'   converted to numeric values, while character vectors and factors are converted
#'   to dummy-variables (numeric 0/1) and are included in the cluster analysis.
#'   If \code{FALSE}, factors and character vectors are removed before computing
#'   the cluster analysis.
#'
#' @inheritParams equivalence_test.lm
#' @inheritParams n_clusters
#'
#' @return The group classification for each observation as vector. The
#'   returned vector includes missing values, so it has the same length
#'   as \code{nrow(x)}.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @details The \code{print()} and \code{plot()} methods show the (standardized)
#'   mean value for each variable within each cluster. Thus, a higher absolute
#'   value indicates that a certain variable characteristic is more pronounced
#'   within that specific cluster (as compared to other cluster groups with lower
#'   absolute mean values).
#'
#' @seealso \code{\link{n_clusters}} to determine the number of clusters to extract, \code{\link{cluster_discrimination}} to determine the accuracy of cluster group classification and \code{\link{check_clusterstructure}} to check suitability of data for clustering.
#'
#' @examples
#' # Hierarchical clustering of mtcars-dataset
#' groups <- cluster_analysis(iris[, 1:4], 3)
#' groups
#'
#' # K-means clustering of mtcars-dataset, auto-detection of cluster-groups
#' \dontrun{
#' groups <- cluster_analysis(iris[, 1:4], method = "k")
#' groups
#' }
#' @export
cluster_analysis <- function(x, n_clusters = NULL, method = c("hclust", "kmeans"),
                             distance = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                             agglomeration = c("ward", "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"),
                             iterations = 20,
                             algorithm = c("Hartigan-Wong", "Lloyd", "MacQueen"),
                             force = TRUE,
                             package = c("NbClust", "mclust"),
                             verbose = TRUE) {
  # match arguments
  distance <- match.arg(distance)
  method <- match.arg(method)
  agglomeration <- match.arg(agglomeration)
  algorithm <- match.arg(algorithm)


  # include factors?
  if (force) {
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


  # check number of clusters
  if (is.null(n_clusters)) {
    n_clusters <- tryCatch(
      {
        nc <- n_clusters(x, package = package, force = force)
        ncs <- attributes(nc)$summary
        n_cl <- ncs$n_Clusters[which.max(ncs$n_Methods)][1]
        if (verbose) {
          insight::print_color(sprintf("Using solution with %i clusters, supported by %i out of %i methods.\n", n_cl, max(ncs$n_Methods), sum(ncs$n_Methods)), "blue")
        }
        n_cl
      },
      error = function(e) {
        if (isTRUE(verbose)) {
          warning(insight::format_message("Could not extract number of cluster. Please provide argument 'n_clusters'."), call. = FALSE)
        }
        1
      }
    )
  }

  # save original data, standardized, for later use
  original_data <- as.data.frame(scale(x))

  # create NA-vector of same length as data frame
  complete.groups <- rep(NA, times = nrow(x))

  # save IDs from non-missing data
  non_missing <- stats::complete.cases(x)
  x <- stats::na.omit(x)

  # Ward Hierarchical Clustering
  if (method == "hclust") {
    # check for argument and R version
    if (agglomeration == "ward") agglomeration <- "ward.D2"
    # distance matrix
    d <- stats::dist(x, method = distance)
    # hierarchical clustering
    hc <- stats::hclust(d, method = agglomeration)
    # cut tree into x clusters
    groups <- stats::cutree(hc, k = n_clusters)
  } else {
    km <- stats::kmeans(x, centers = n_clusters, iter.max = iterations, algorithm = algorithm)
    # return cluster assignment
    groups <- km$cluster
  }

  # create vector with cluster group classification,
  # including missings
  complete.groups[non_missing] <- groups

  # create mean of z-score for each variable in data
  out <- as.data.frame(do.call(rbind, lapply(original_data, tapply, complete.groups, mean)))
  colnames(out) <- sprintf("Group %s", colnames(out))
  out <- cbind(data.frame(Term = rownames(out), stringsAsFactors = FALSE), out)
  rownames(out) <- NULL

  attr(complete.groups, "data") <- out
  attr(complete.groups, "accuracy") <- tryCatch(
    {
      cluster_discrimination(original_data, complete.groups)
    },
    error = function(e) {
      NULL
    }
  )

  class(complete.groups) <- c("cluster_analysis", "see_cluster_analysis", class(complete.groups))

  complete.groups
}




#' @export
print.cluster_analysis <- function(x, digits = 2, ...) {
  # retrieve data
  dat <- attr(x, "data", exact = TRUE)

  if (is.null(dat)) {
    stop("Could not find data frame that was used for cluster analysis.", call. = FALSE)
  }

  # save output from cluster_discrimination()
  accuracy <- attributes(x)$accuracy

  # headline
  insight::print_color("# Cluster Analysis (mean z-score by cluster)\n\n", "blue")

  # round numeric variables (i.e. all but first term column)
  dat[2:ncol(dat)] <- sapply(dat[2:ncol(dat)], round, digits = digits)
  print.data.frame(dat, row.names = FALSE)

  if (!is.null(accuracy)) {
    cat("\n")
    print(accuracy)
  }
  invisible(x)
}
