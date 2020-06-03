#' @rdname model_parameters.default
#' @export
model_parameters.logitor <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, standardize = NULL, exponentiate = TRUE, robust = FALSE, p_adjust = NULL, ...) {
  model_parameters.default(model$fit, ci = ci, bootstrap = bootstrap, iterations = iterations, standardize = standardize, exponentiate = exponentiate, robust = robust, p_adjust = p_adjust, ...)
}

#' @export
model_parameters.poissonirr <- model_parameters.logitor

#' @export
model_parameters.negbinirr <- model_parameters.logitor




#' @rdname model_parameters.default
#' @export
model_parameters.poissonmfx <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "marginal"), standardize = NULL, exponentiate = FALSE, robust = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    robust = robust,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}

#' @export
model_parameters.logitmfx <- model_parameters.poissonmfx

#' @export
model_parameters.probitmfx <- model_parameters.poissonmfx

#' @export
model_parameters.negbinmfx <- model_parameters.poissonmfx





#' @export
model_parameters.betaor <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("conditional", "precision", "all"), standardize = NULL, exponentiate = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  model_parameters.betareg(model$fit, ci = ci, bootstrap = bootstrap, iterations = iterations, component = component, standardize = standardize, exponentiate = exponentiate, p_adjust = p_adjust, ...)
}

#' @rdname model_parameters.default
#' @export
model_parameters.betamfx <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "precision", "marginal"), standardize = NULL, exponentiate = FALSE, robust = FALSE, p_adjust = NULL, ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    robust = robust,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}

