#' Number of Clusters to Extract
#'
#' This function runs many existing procedures for determining how many clusters are present in your data. It returns the number of clusters based on the maximum consensus. In case of ties, it will select the solution with the less clusters.
#'
#' @inheritParams n_factors
#'
#' @examples
#' x <- iris[, 1:4]
#' @export
n_clusters <- function(x, ...) {
  x <- x[sapply(x, is.numeric)]
  x <- standardize(x)

  # mclust <- .n_clusters_mclust(x)
  # NbClust <- .n_clusters_NbClust(x)

  # Add summary
  # by_factors <- .data_frame(
  #   n_Factors = as.numeric(unique(out$n_Factors)),
  #   n_Methods = as.numeric(by(out, as.factor(out$n_Factors), function(out) n <- nrow(out)))
  # )
  #
  # attr(out, "by_factors") <- by_factors
  # attr(out, "n") <- min(as.numeric(as.character(by_factors[by_factors$n_Methods == max(by_factors$n_Methods), c("n_Factors")])))
  #
  # out
}





#' @keywords internal
.n_clusters_mclust <- function(x, ...) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' required for this function to work. Please install it by running `install.packages('mclust')`.")
  }
  mclustBIC <- mclust::mclustBIC # this is needed as it is internally required by the following function
  BIC <- mclust::mclustBIC(x, verbose = FALSE)
  out <- data.frame(unclass(BIC))
  n <- which(out == max(out), arr.ind = TRUE)[1]
  data.frame(n_Clusters = n, Method = "Mixture", Package = "mclust")
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
    validation = c("internal", "stability")
  )
  out <- clValid::optimalScores(rez)
  data.frame(n_Clusters = out$Clusters, Method = row.names(out), Package = "clValid")
}


#' @importFrom grDevices png dev.off
#' @keywords internal
.n_clusters_NbClust <- function(x, ...) {
  if (!requireNamespace("NbClust", quietly = TRUE)) {
    stop("Package 'NbClust' required for this function to work. Please install it by running `install.packages('NbClust')`.")
  }

  # Run the function and suppress output and automatic plotting
  ff <- tempfile()
  grDevices::png(filename = ff)
  junk <- capture.output(n <- NbClust::NbClust(x, min.nc = 2, max.nc = 9, method = "ward.D2", index = "alllong"))
  grDevices::dev.off()
  unlink(ff)

  out <- as.data.frame(t(n$Best.nc))
  data.frame(n_Clusters = out$Number_clusters, Method = row.names(out), Package = "NbClust")
}
