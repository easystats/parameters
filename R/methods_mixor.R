
#' @rdname model_parameters.merMod
#' @export
model_parameters.mixor <- function(model,
                                   ci = .95,
                                   effects = c("all", "fixed", "random"),
                                   bootstrap = FALSE,
                                   iterations = 1000,
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   details = FALSE,
                                   verbose = TRUE,
                                   ...) {
  effects <- match.arg(effects)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Effects"),
    standardize = standardize,
    exponentiate = exponentiate,
    effects = effects,
    robust = FALSE,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(details)) {
    attr(out, "details") <- .randomeffects_summary(model)
  }

  out
}


#' @rdname ci.merMod
#' @export
ci.mixor <- function(x, ci = .95, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  ci_wald(model = x, ci = ci, dof = Inf, effects = effects, robust = FALSE, ...)
}


#' @rdname standard_error
#' @importFrom insight get_parameters
#' @export
standard_error.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "Std. Error"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @rdname p_value.lmerMod
#' @importFrom insight get_parameters
#' @export
p_value.mixor <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  stats <- model$Model[, "P(>|z|)"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @export
simulate_model.mixor <- function(model, iterations = 1000, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  out <- .simulate_model(model, iterations, component = "conditional", effects = effects)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}
