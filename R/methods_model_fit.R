## tidymodels (.model_fit)

# model parameters ---------------------


#' @export
model_parameters.model_fit <- function(model,
                                       ci = .95,
                                       effects = "fixed",
                                       component = "conditional",
                                       ci_method = "profile",
                                       bootstrap = FALSE,
                                       iterations = 1000,
                                       standardize = NULL,
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       verbose = TRUE,
                                       ...) {
  model_parameters(
    model$fit,
    ci = ci,
    effects = effects,
    component = component,
    ci_method = ci_method,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )
}



# ci ------------------


#' @export
ci.model_fit <- function(x, ci = .95, method = NULL, ...) {
  ci(x$fit, ci = ci, method = method, ...)
}



# standard error ------------------


#' @export
standard_error.model_fit <- function(model, ...) {
  standard_error(model$fit, ...)
}



# degrees of freedom ------------------


#' @export
degrees_of_freedom.model_fit <- function(model, ...) {
  degrees_of_freedom(model$fit, ...)
}



# p values ------------------


#' @export
p_value.model_fit <- function(model, ...) {
  p_value(model$fit, ...)
}



# simulate model ------------------


#' @export
simulate_model.model_fit <- function(model, iterations = 1000, ...) {
  simulate_model(model$fit, iterations = iterations, ...)
}
