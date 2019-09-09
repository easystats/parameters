#' Parameters of Generalized Additive (Mixed) Models
#'
#' Extract and compute indices and measures to describe parameters of generalized additive models (GAM(M)s).
#'
#' @param model A gam/gamm model.
#' @inheritParams model_parameters.lm
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' library(parameters)
#' library(mgcv)
#'
#' dat <- gamSim(1, n = 400, dist = "normal", scale = 2)
#' model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat)
#' model_parameters(model)
#' @export
model_parameters.gam <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = "all", merge_by = c("Parameter", "Component"))
  }

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}



#' @export
model_parameters.gamm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  model <- model$gam
  class(model) <- c("gam", "lm", "glm")
  model_parameters(model, ci = ci, bootstrap = bootstrap, iterations = iterations, ...)
}


#' @export
model_parameters.list <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {
  if ("gam" %in% names(model)) {
    model <- model$gam
    class(model) <- c("gam", "lm", "glm")
    model_parameters(model, ci = ci, bootstrap = bootstrap, iterations = iterations, ...)
  }
}


#' @export
model_parameters.gamlss <- model_parameters.gam

