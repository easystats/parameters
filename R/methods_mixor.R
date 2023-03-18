#' @rdname model_parameters.merMod
#' @export
model_parameters.mixor <- function(model,
                                   ci = 0.95,
                                   effects = "all",
                                   bootstrap = FALSE,
                                   iterations = 1000,
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   verbose = TRUE,
                                   include_sigma = FALSE,
                                   ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))

  # standardize only works for fixed effects...
  if (!is.null(standardize) && standardize != "refit") {
    if (!missing(effects) && effects != "fixed" && verbose) {
      insight::format_warning(
        "Standardizing coefficients only works for fixed effects of the mixed model."
      )
    }
    effects <- "fixed"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Effects"),
    standardize = standardize,
    exponentiate = exponentiate,
    effects = effects,
    include_sigma = include_sigma,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))

  out
}


#' @export
ci.mixor <- function(x, ci = 0.95, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  .ci_generic(model = x, ci = ci, dof = Inf, effects = effects, ...)
}


#' @export
standard_error.mixor <- function(model, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  stats <- model$Model[, "Std. Error"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    SE = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @export
p_value.mixor <- function(model, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  stats <- model$Model[, "P(>|z|)"]
  parms <- insight::get_parameters(model, effects = effects)

  .data_frame(
    Parameter = parms$Parameter,
    p = stats[parms$Parameter],
    Effects = parms$Effects
  )
}


#' @export
simulate_model.mixor <- function(model, iterations = 1000, effects = "all", ...) {
  effects <- match.arg(effects, choices = c("all", "fixed", "random"))
  out <- .simulate_model(model, iterations, component = "conditional", effects = effects, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}
