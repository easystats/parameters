#' @export
standard_error.parameters_skewness <- function(model, ...) {
  # datawizard returns SE as column, older objects stored it as attribute
  if ("SE" %in% names(model)) {
    model$SE
  } else {
    attributes(model)$SE
  }
}


#' @export
standard_error.parameters_kurtosis <- standard_error.parameters_skewness

#' @export
standard_error.datawizard_skewness <- standard_error.parameters_skewness

#' @export
standard_error.datawizard_kurtosis <- standard_error.parameters_skewness
