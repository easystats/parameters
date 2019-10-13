#' Count how many parameters in a model
#'
#' Returns the number of parameters of a model.
#'
#' @param x A statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @return The number of parameters in the model.
#' @export
n_parameters <- function(x, ...) {
  UseMethod("n_parameters")
}

#' @export
n_parameters.default <- function(x, ...) {
  length(insight::find_parameters(x, effects = "fixed", flatten = TRUE, ...))
}
