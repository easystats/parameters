#' Number of Clusters to Extract
#'
#' This function runs many existing procedures for determining how many clusters are present in your data. It returns the number of clusters based on the maximum consensus. In case of ties, it will select the solution with the less clusters.
#'
#' @inheritParams check_clusterstructure
#' @param package These are the packages from which methods are used. Can be \code{"all"} or a vector containing \code{"NbClust"}, \code{"mclust"}, \code{"clValid"} and \code{"cluster"}.
#' @param fast If \code{FALSE}, will compute 4 more indices (sets \code{index = "allong"} in \code{NbClust}). This has been deactivated by default as it is computationaly heavy.
#'
#' @examples
#' library(parameters)
#' \donttest{
#' n_clusters(iris[, 1:4])}
#' @export
n_clusters <- function(x, standardize = TRUE, package = c("NbClust", "mclust", "clValid", "cluster"), fast = TRUE, ...) {

  if (all(package == "all")) {
    package <- c("NbClust", "mclust", "clValid", "cluster")
  }

  x <- as.data.frame(x[sapply(x, is.numeric)])
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
  if ("clvalid" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_clValid(x))
  }
  if ("cluster" %in% tolower(package)) {
    out <- rbind(out, .n_clusters_cluster(x))
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
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' required for this function to work. Please install it by running `install.packages('mclust')`.")
  }
  mclustBIC <- mclust::mclustBIC # this is needed as it is internally required by the following function
  BIC <- mclust::mclustBIC(x, verbose = FALSE)
  out <- data.frame(unclass(BIC))
  n <- which(out == max(out, na.rm = TRUE), arr.ind = TRUE)[1]
  data.frame(n_Clusters = n, Method = "Mixture", Package = "mclust")
}





#' @importFrom utils capture.output
#' @keywords internal
.n_clusters_cluster <- function(x, ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package 'cluster' required for this function to work. Please install it by running `install.packages('cluster')`.")
  }

  # listwise deletion of missing
  x <- stats::na.omit(x)
  # Gap Statistic for Estimating the Number of Clusters
  junk <- utils::capture.output(gap <- cluster::clusGap(x, kmeans, K.max = 10, B = 100))
  # Gap Statistic for Estimating the Number of Clusters
  n <- cluster::maxSE(f = gap[, "gap"], SE.f = gap[, "SE.sim"], method = "Tibs2001SEmax", SE.factor = 1)
  n <- cluster::maxSE(f = gap[, "gap"], SE.f = gap[, "SE.sim"], method = "Tibs2001SEmax", SE.factor = 1)
  data.frame(n_Clusters = n, Method = "Tibs2001SEmax", Package = "cluster")
}






#' @keywords internal
.n_clusters_clValid <- function(x, ...) {
  if (!requireNamespace("clValid", quietly = TRUE)) {
    stop("Package 'clValid' required for this function to work. Please install it by running `install.packages('clValid')`.")
  }

  row.names(x) <- paste0("row", row.names(x))
  rez <- clValid::clValid(x,
    nClust = 2:9,
    # clMethods = c("hierarchical", "kmeans", "pam", "diana", "fanny", "som", "model", "sota", "clara", "agnes"),
    clMethods = c("hierarchical", "kmeans", "pam", "diana", "clara", "agnes"),
    validation = c("internal", "stability"),
    maxitems = nrow(x)
  )
  out <- clValid::optimalScores(rez)
  data.frame(n_Clusters = out$Clusters, Method = row.names(out), Package = "clValid")
}







#' @importFrom grDevices png dev.off
#' @keywords internal
.n_clusters_NbClust <- function(x, fast = TRUE, ...) {
  if (!requireNamespace("NbClust", quietly = TRUE)) {
    stop("Package 'NbClust' required for this function to work. Please install it by running `install.packages('NbClust')`.")
  }

  # Run the function and suppress output and automatic plotting
  ff <- tempfile()
  grDevices::png(filename = ff)
  if (fast) {
    indices <- "all"
  } else{
    indices <- "allong"
  }
  junk <- utils::capture.output(n <- NbClust::NbClust(x, min.nc = 2, max.nc = 9, method = "ward.D2", index = indices))
  grDevices::dev.off()
  unlink(ff)

  out <- as.data.frame(t(n$Best.nc))
  data.frame(n_Clusters = out$Number_clusters, Method = row.names(out), Package = "NbClust")
}
