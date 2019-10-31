#' @export
model_parameters.wbm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = "conditional", merge_by = c("Parameter", "Component"))
  }

  parameters <- .add_model_parameters_attributes(parameters, model, ci, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}


#' @export
model_parameters.wbgee <- model_parameters.wbm