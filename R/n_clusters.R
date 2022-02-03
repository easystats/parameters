#' Find number of clusters in your data
#'
#' Similarly to [n_factors()] for factor / principal component analysis,
#' `n_clusters` is the main function to find out the optimal numbers of clusters
#' present in the data based on the maximum consensus of a large number of
#' methods.
#' \cr
#' Essentially, there exist many methods to determine the optimal number of
#' clusters, each with pros and cons, benefits and limitations. The main
#' `n_clusters` function proposes to run all of them, and find out the number of
#' clusters that is suggested by the majority of methods (in case of ties, it
#' will select the most parsimonious solution with fewer clusters).
#' \cr
#' Note that we also implement some specific, commonly used methods, like the
#' Elbow or the Gap method, with their own visualization functionalities. See
#' the examples below for more details.
#'
#' @inheritParams check_clusterstructure
#' @param include_factors Logical, if `TRUE`, factors are converted to numerical
#'   values in order to be included in the data for determining the number of
#'   clusters. By default, factors are removed, because most methods that
#'   determine the number of clusters need numeric input only.
#' @param package Package from which methods are to be called to determine the
#'   number of clusters. Can be `"all"` or a vector containing
#'   `"easystats"`, `"NbClust"`, `"mclust"`, and `"M3C"`.
#' @param fast If `FALSE`, will compute 4 more indices (sets `index = "allong"`
#'   in `NbClust`). This has been deactivated by default as it is
#'   computationally heavy.
#' @param n_max Maximal number of clusters to test.
#' @param clustering_function,gap_method Other arguments passed to other
#'   functions. `clustering_function` is used by `fviz_nbclust` and
#'   can be `kmeans`, code{cluster::pam}, code{cluster::clara},
#'   code{cluster::fanny}, and more. `gap_method` is used by
#'   `cluster::maxSE` to extract the optimal numbers of clusters (see its
#'   `method` argument).
#' @param method,min_size,eps_n,eps_range Arguments for DBSCAN algorithm.
#' @param distance_method The distance method (passed to [dist()]). Used by
#'   algorithms relying on the distance matrix, such as `hclust` or `dbscan`.
#' @param hclust_method The hierarchical clustering method (passed to [hclust()]).
#' @param nbclust_method The clustering method (passed to `NbClust::NbClust()`
#'   as `method`).
#' @inheritParams model_parameters.glm
#'
#'
#'
#' @note There is also a [`plot()`-method](https://easystats.github.io/see/articles/parameters.html) implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' \dontrun{
#' library(parameters)
#'
#' # The main 'n_clusters' function ===============================
#' if (require("mclust", quietly = TRUE) && require("NbClust", quietly = TRUE) &&
#'   require("cluster", quietly = TRUE) && require("see", quietly = TRUE)) {
#'   n <- n_clusters(iris[, 1:4], package = c("NbClust", "mclust")) # package can be "all"
#'   n
#'   summary(n)
#'   as.data.frame(n)  # Duration is the time elapsed for each method in seconds
#'   plot(n)
#'
#'   # The following runs all the method but it significantly slower
#'   # n_clusters(iris[1:4], standardize = FALSE, package = "all", fast = FALSE)
#' }
#' }
#' @export
n_clusters <- function(x,
                       standardize = TRUE,
                       include_factors = FALSE,
                       package = c("easystats", "NbClust", "mclust"),
                       fast = TRUE,
                       nbclust_method = "kmeans",
                       n_max = 10,
                       ...) {
  if (all(package == "all")) {
    package <- c("easystats", "NbClust", "mclust", "M3C")
  }

  x <- .prepare_data_clustering(x, include_factors = include_factors, standardize = standardize, ...)

  out <- data.frame()

  if ("easystats" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_easystats(x, n_max = n_max, ...))
  }

  if ("nbclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_NbClust(x, fast = fast, nbclust_method = nbclust_method, n_max = n_max, ...))
  }

  if ("mclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_mclust(x, n_max = n_max, ...))
  }

  if ("M3C" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_M3C(x, n_max = n_max, fast = fast))
  }

  # Drop Nans
  out <- out[!is.na(out$n_Clusters), ]

  # Error if no solution
  if(nrow(out) == 0) stop("No complete solution was found. Please try again with more methods.")

  # Clean
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
.n_clusters_mclust <- function(x, n_max = 10, ...) {
  insight::check_if_installed("mclust")
  t0 <- Sys.time()
  mclustBIC <- mclust::mclustBIC # this is needed as it is internally required by the following function
  BIC <- mclust::mclustBIC(x, G = 1:n_max, verbose = FALSE)
  # Extract the best solutions as shown in summary(BIC)
  out <- strsplit(names(unclass(summary(BIC))), split = ",", fixed = TRUE)
  # Get separated vectors
  models <- as.character(sapply(out, function(x) x[[1]]))
  n <- as.numeric(sapply(out, function(x) x[[2]]))

  data.frame(n_Clusters = n,
             Method = paste0("Mixture (", models, ")"),
             Package = "mclust",
             Duration = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}




# Methods -----------------------------------------------------------------


#' @keywords internal
.n_clusters_easystats <- function(x, n_max = 10, ...) {
  elb <- n_clusters_elbow(x, preprocess = FALSE, n_max = n_max, ...)
  sil <- n_clusters_silhouette(x, preprocess = FALSE, n_max = n_max, ...)
  gap1 <- n_clusters_gap(x, preprocess = FALSE, gap_method = "firstSEmax", n_max = n_max, ...)
  gap2 <- n_clusters_gap(x, preprocess = FALSE, gap_method = "globalSEmax", n_max = n_max, ...)

  data.frame(
    n_Clusters = c(attributes(elb)$n, attributes(sil)$n, attributes(gap1)$n, attributes(gap2)$n),
    Method = c("Elbow", "Silhouette", "Gap_Maechler2012", "Gap_Dudoit2002"),
    Package = "easystats",
    Duration = c(attributes(elb)$duration, attributes(sil)$duration, attributes(gap1)$duration, attributes(gap2)$duration)
  )
}




#' @keywords internal
.n_clusters_NbClust <- function(x, fast = TRUE, nbclust_method = "kmeans", n_max = 10, indices = "all", ...) {
  insight::check_if_installed("NbClust")

  if (all(indices == "all")) {
    indices <- c("kl", "Ch", "Hartigan", "CCC", "Scott", "Marriot", "trcovw", "Tracew", "Friedman", "Rubin", "Cindex", "DB", "Silhouette", "Duda", "Pseudot2", "Beale", "Ratkowsky", "Ball", "PtBiserial", "Frey", "Mcclain", "Dunn", "SDindex", "SDbw", "gap", "gamma", "gplus", "tau")
    # c("hubert", "dindex") are graphical methods
  }
  if (fast) {
    indices <- indices[!indices %in% c("gap", "gamma", "gplus", "tau")]
  }

  out <- data.frame()
  for (idx in indices) {
    t0 <- Sys.time()
    n <- tryCatch(
      expr = {
        .catch_warnings(NbClust::NbClust(
          x,
          index = tolower(idx),
          method = nbclust_method,
          max.nc = n_max,
          ...
        ))
      },
      error = function(e) {
        NULL
      }
    )

    if(!is.null(n)) {
      # Catch and print potential warnings
      w <- ""
      if(!is.null(n$warnings)) {
        w <- paste0("\n  - ", unlist(n$warnings), collapse = "")
        warning(paste0("For ", idx, " index (NbClust):", w), call. = FALSE)
      }

      # Don't merge results if convergence issue
      if(grepl("did not converge in", w) == FALSE) {
        out <- rbind(out, data.frame(
          n_Clusters = n$out$Best.nc[["Number_clusters"]],
          Method = idx,
          Package = "NbClust",
          Duration = as.numeric(difftime(Sys.time(), t0, units = "secs"))
        ))
      }
    }
  }
  out
}


#' @keywords internal
.n_clusters_M3C <- function(x, n_max = 10, fast = TRUE, ...) {
  if (!requireNamespace("M3C", quietly = TRUE)) {
    stop("Package 'M3C' required for this function to work. Please install it by first running `remotes::install_github('https://github.com/crj32/M3C')` (the package is not on CRAN).") # Not on CRAN (but on github and bioconductor)
  }

  data <- data.frame(t(x))
  colnames(data) <- paste0("x", seq(1, ncol(data))) # Add columns names as required by the package

  t0 <- Sys.time()
  out <- M3C::M3C(data, method = 2, maxK = n_max, removeplots = TRUE, silent = TRUE)

  out <- data.frame(
    n_Clusters = out$scores[which.min(out$scores$PCSI), "K"],
    Method = "Consensus clustering algorithm (penalty term)",
    Package = "M3C",
    Duration = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )

  # Monte Carlo Version (Super slow)
  if (fast == FALSE){
    t0 <- Sys.time()
    out2 <- M3C::M3C(data, method=1, maxK = n_max, removeplots = TRUE, silent = TRUE)
    out <- rbind(out,
                 data.frame(n_Clusters = out2$scores[which.max(out2$scores$RCSI), "K"],
                            Method = "Consensus clustering algorithm (Monte Carlo)",
                            Package = "M3C",
                            Duration = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  out
}



