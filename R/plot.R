#' @export
plot.parameters_sem <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot SEM and CFA graphs. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
