#' @export
standard_error.parameters_skewness <- function(model, ...) {
  attributes(model)$SE
}


#' @export
standard_error.parameters_kurtosis <- standard_error.parameters_skewness

