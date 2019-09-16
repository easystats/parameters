#' @inheritParams model_simulate
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, standardize = FALSE, standardize_robust = FALSE, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
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

  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
    standardize <- "refit"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    parameters <- cbind(parameters, parameters_standardize(model, method = standardize, robust = standardize_robust, verbose = FALSE)[2])
  }

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB

