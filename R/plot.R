#' @export
plot.parameters_sem <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot SEM and CFA graphs. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.parameters_model <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot model parameters. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.parameters_simulate <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.n_factors <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.n_clusters <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}


#' @export
plot.parameters_pca <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot point-estimates. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
