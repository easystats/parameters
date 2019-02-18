#' Computation of p-values
#'
#' Computes p-values related to parameters.
#'
#' @param model Statistical model.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
p_value <- function(model, ...) {
  UseMethod("p_value")
}
