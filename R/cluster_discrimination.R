#' Compute a linear discriminant analysis on classified cluster groups
#'
#' Computes linear discriminant analysis (LDA) on classified cluster groups, and determines the goodness of classification for each cluster group. See `MASS::lda()` for details.
#'
#' @param x A data frame
#' @param cluster_groups Group classification of the cluster analysis, which can
#'   be retrieved from the [cluster_analysis()] function.
#' @param ... Other arguments to be passed to or from.
#'
#' @seealso [n_clusters()] to determine the number of clusters to extract, [cluster_analysis()] to compute a cluster analysis and [check_clusterstructure()] to check suitability of data for clustering.
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   # Retrieve group classification from hierarchical cluster analysis
#'   clustering <- cluster_analysis(iris[, 1:4], n = 3)
#'
#'   # Goodness of group classification
#'   cluster_discrimination(clustering)
#' }
#' @export
cluster_discrimination <- function(x, cluster_groups = NULL, ...) {
  UseMethod("cluster_discrimination")
}

#' @export
cluster_discrimination.cluster_analysis <- function(x, cluster_groups = NULL, ...) {
  if (is.null(cluster_groups)) {
    cluster_groups <- stats::predict(x)
  }
  cluster_discrimination(attributes(x)$data, cluster_groups, ...)
}



#' @export
cluster_discrimination.default <- function(x, cluster_groups = NULL, ...) {
  if (is.null(cluster_groups)) {
    stop("Please provide cluster assignments via 'cluster_groups'.")
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

  # Sort according to accuracy
  out <- out[order(out$Group), ]

  attr(out, "Overall_Accuracy") <- total_correct
  class(out) <- c("cluster_discrimination", class(out))

  out
}


# Utils -------------------------------------------------------------------


#' @export
print.cluster_discrimination <- function(x, ...) {
  orig_x <- x
  insight::print_color("# Accuracy of Cluster Group Classification via Linear Discriminant Analysis (LDA)\n\n", "blue")

  total_accuracy <- attributes(x)$Overall_Accuracy
  x$Accuracy <- sprintf("%.2f%%", 100 * x$Accuracy)
  total <- sprintf("%.2f%%", 100 * total_accuracy)

  print.data.frame(x, row.names = FALSE, ...)
  insight::print_color(sprintf("\nOverall accuracy of classification: %s\n", total), "yellow")
  invisible(orig_x)
}
