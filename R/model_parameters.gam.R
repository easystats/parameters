#' Parameters of Generalized Additive (Mixed) Models
#'
#' Extract and compute indices and measures to describe parameters of generalized additive models (GAM(M)s).
#'
#' @param model A gam/gamm model.
#' @inheritParams model_parameters.default
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
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
model_parameters.gam <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = FALSE, ...) {
  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = "all", merge_by = c("Parameter", "Component"), standardize = standardize)
  }

  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.vgam <- model_parameters.gam


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



#' @rdname model_parameters.gam
#' @export
model_parameters.rqss <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("conditional", "smooth_terms", "all"), standardize = NULL, exponentiate = FALSE, ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @rdname model_parameters.gam
#' @export
model_parameters.cgam <- model_parameters.rqss
