#' @export
standard_error.lm_robust <- function(model, ...) {
  if (insight::is_multivariate(model)) {
    standard_error.mlm(model, ...)
  } else {
    standard_error.default(model, ...)
  }
}


#' @export
p_value.lm_robust <- function(model, ...) {
  if (insight::is_multivariate(model)) {
    p_value.mlm(model, ...)
  } else {
    p_value.default(model, ...)
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
model_parameters.lm_robust <- function(model, ...) {
  if (insight::is_multivariate(model)) {
    model_parameters.mlm(model, ...)
  } else {
    model_parameters.default(model, ...)
  }
}
