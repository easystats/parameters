#' Model bootstrapping
#'
#' Bootstrap the model n times to return a data.frame of estimates.
#'
#' @param model Statistical model.
#' @param n The number of bootstrap replicates.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @export
model_bootstrap <- function(model, n = 1000, ...) {
  UseMethod("model_bootstrap")
}
