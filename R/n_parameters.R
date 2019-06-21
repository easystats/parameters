#' Count how many parameters in a model
#'
#' Returns the number of parameters of a model.
#'
#' @param x A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
n_parameters <- function(x, ...) {
  length(insight::find_parameters(x, flatten = TRUE, ...))
}
