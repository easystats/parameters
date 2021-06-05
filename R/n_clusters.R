#' Number of clusters to extract
#'
#' This function runs many existing procedures for determining how many clusters
#' are present in data. It returns the number of clusters based on the maximum
#' consensus. In case of ties, it will select the solution with fewer clusters.
#'
#' @inheritParams check_clusterstructure
#' @param force Logical, if \code{TRUE}, factors are converted to numerical
#'   values in order to be included in the data for determining the number of
#'   clusters. By default, factors are removed, because most methods that
#'   determine the number of clusters need numeric input only.
#' @param package Package from which methods are to be called to determine the
#'   number of clusters. Can be \code{"all"} or a vector containing
#'   \code{"NbClust"}, \code{"mclust"}, \code{"cluster"} and \code{"M3C"}.
#' @param fast If \code{FALSE}, will compute 4 more indices (sets \code{index =
#'   "allong"} in \code{NbClust}). This has been deactivated by default as it is
#'   computationally heavy.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/parameters.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' library(parameters)
#' \donttest{
#' if (require("mclust", quietly = TRUE) && require("NbClust", quietly = TRUE) &&
#'   require("cluster", quietly = TRUE)) {
#'   n_clusters(iris[, 1:4], package = c("NbClust", "mclust", "cluster"))
#' }
#' }
#' @export
n_clusters <- function(x,
                       standardize = TRUE,
                       force = FALSE,
                       package = c("NbClust", "mclust", "cluster", "M3C"),
                       fast = TRUE,
                       ...) {
  if (all(package == "all")) {
    package <- c("NbClust", "mclust", "cluster", "M3C")
  }

  # convert factors to numeric
  if (force) {
    factors <- sapply(x, function(i) is.character(i) | is.factor(i))
    if (any(factors)) x[factors] <- sapply(x[factors], .factor_to_numeric)
  }

  # remove all missing values from data, only use numerics
  x <- stats::na.omit(as.data.frame(x[sapply(x, is.numeric)]))
  if (standardize) {
    x <- as.data.frame(scale(x))
  }

  out <- data.frame()

  if ("nbclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_NbClust(x, fast = fast))
  }

  if ("mclust" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_mclust(x))
  }

  if ("cluster" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_cluster(x))
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


#' @keywords internal
.n_clusters_cluster <- function(x, ...) {
  insight::check_if_installed("cluster")

  # listwise deletion of missing
  x <- stats::na.omit(x)

  # Gap Statistic for Estimating the Number of Clusters
  junk <- utils::capture.output(gap <- cluster::clusGap(x,
    stats::kmeans,
    K.max = 10,
    B = 100
  )$Tab)

  # Gap Statistic for Estimating the Number of Clusters
  n <- cluster::maxSE(
    f = gap[, "gap"],
    SE.f = gap[, "SE.sim"],
    method = "Tibs2001SEmax",
    SE.factor = 1
  )

  data.frame(n_Clusters = n, Method = "Tibs2001SEmax", Package = "cluster")
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
    indices <- "allong"
  }

  junk <- utils::capture.output(n <- NbClust::NbClust(
    x,
    min.nc = 2,
    max.nc = 9,
    method = "ward.D2",
    index = indices
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
