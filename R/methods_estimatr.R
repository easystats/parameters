#' @export
standard_error.lm_robust <- function(x, ...) {
  if (insight::is_multivariate(x)) {
    standard_error.mlm(x, ...)
  } else {
    standard_error.default(x, ...)
  }
}


#' @export
p_value.lm_robust <- function(x, ...) {
  if (insight::is_multivariate(x)) {
    p_value.mlm(x, ...)
  } else {
    p_value.default(x, ...)
  }
}


#' @export
ci.lm_robust <- function(x, ...) {
  if (insight::is_multivariate(x)) {
    ci.mlm(x, ...)
  } else {
    ci.default(x, ...)
  }
}


#' @export
model_parameters.lm_robust <- function(x, ...) {
  if (insight::is_multivariate(x)) {
    model_parameters.mlm(x, ...)
  } else {
    model_parameters.default(x, ...)
  }
}
