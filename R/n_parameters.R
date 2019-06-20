# Count how many parameters in a model
#
#' @export
n_parameters <- function(x, ...) {
  length(insight::find_parameters(x, flatten = TRUE, ...))
}

