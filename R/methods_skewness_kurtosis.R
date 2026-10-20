#' @export
standard_error.parameters_skewness <- function(model, ...) {
  # datawizard returns a data frame with an "SE" column; objects from early
  # versions of skewness() and kurtosis() in parameters, before these functions
  # moved to datawizard, stored the standard error in an attribute
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
