# model_parameters ----------------

#' @export
model_parameters.estimate_means <- function(model, ...) {
  out <- model
  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}

#' @export
model_parameters.estimate_slopes <- model_parameters.estimate_means

#' @export
model_parameters.estimate_contrasts <- model_parameters.estimate_means


# standard_error ----------------

#' @export
standard_error.estimate_means <- function(model, ...) {
  params <- insight::get_parameters(model)
  data.frame(
    Parameter = params$Parameter,
    SE = model$SE,
    stringsAsFactors = FALSE
  )
}

#' @export
standard_error.estimate_slopes <- standard_error.estimate_means

#' @export
standard_error.estimate_contrasts <- standard_error.estimate_means


# ci ----------------

#' @export
ci.estimate_means <- function(model, ...) {
  params <- insight::get_parameters(model)

  ci_value <- attributes(model)$ci
  if (is.null(ci_value)) {
    ci_value <- 0.95
  }

  data.frame(
    Parameter = params$Parameter,
    CI = ci_value,
    CI_low = model$CI_low,
    CI_high = model$CI_high,
    stringsAsFactors = FALSE
  )
}

#' @export
ci.estimate_slopes <- ci.estimate_means

#' @export
ci.estimate_contrasts <- ci.estimate_means
