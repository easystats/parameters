#' @title Compute a linear discriminant analysis on classified cluster groups
#'
#' @name cluster_discrimination
#'
#' @description Computes linear discriminant analysis on classified cluster groups,
#'   and determines the goodness of classification for each cluster group.
#'
#' @param x A data frame
#' @param cluster_groups Group classification of the cluster analysis, which can
#'   be retrieved from the [cluster_analysis()] function.
#'
#' @seealso [n_clusters()] to determine the number of clusters to extract, [cluster_analysis()] to compute a cluster analysis and [check_clusterstructure()] to check suitability of data for clustering.
#'
#' @examples
#' \dontrun{
#' # retrieve group classification from hierarchical cluster analysis
#' groups <- cluster_analysis(iris[, 1:4])
#'
#' # goodness of group classificatoin
#' cluster_discrimination(iris[, 1:4], cluster_groups = groups)
#' }
#' @export
cluster_discrimination <- function(x, cluster_groups) {
  if (is.null(cluster_groups)) {
    cluster_groups <- cluster_analysis(x)
  }

  x <- stats::na.omit(x)
  cluster_groups <- stats::na.omit(cluster_groups)

  # compute discriminant analysis of groups on original data frame
  insight::check_if_installed("MASS")
  disc <- MASS::lda(cluster_groups ~ ., data = x, na.action = "na.omit", CV = TRUE)

  # Assess the accuracy of the prediction
  # percent correct for each category of groups
  classification_table <- table(cluster_groups, disc$class)
  correct <- diag(prop.table(classification_table, 1))

  # total correct percentage
  total_correct <- sum(diag(prop.table(classification_table)))

  out <- data.frame(
    Group = unique(cluster_groups),
    Accuracy = correct,
    stringsAsFactors = FALSE
  )

  attr(out, "Overall_Accuracy") <- total_correct
  class(out) <- c("cluster_discrimination", class(out))

  out
}


#' @export
print.cluster_discrimination <- function(x, ...) {
  orig_x <- x
  insight::print_color("# Accuracy of Cluster Group Classification\n\n", "blue")

  total_accuracy <- attributes(x)$Overall_Accuracy
  x$Accuracy <- sprintf("%.2f%%", 100 * x$Accuracy)
  total <- sprintf("%.2f%%", 100 * total_accuracy)

  print.data.frame(x, row.names = FALSE, ...)
  insight::print_color(sprintf("\nOverall accuracy of classification: %s\n", total), "yellow")
  invisible(orig_x)
}
