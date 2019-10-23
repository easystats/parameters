#' @inheritParams model_simulate
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = component, ...)
  }


  parameters <- .add_model_parameters_attributes(parameters, model, ci, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB
