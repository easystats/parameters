#' @export
model_parameters.glmm <- function(model,
                                  ci = 0.95,
                                  effects = c("all", "fixed", "random"),
                                  bootstrap = FALSE,
                                  iterations = 1000,
                                  standardize = NULL,
                                  exponentiate = FALSE,
                                  keep = NULL,
                                  drop = NULL,
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
    keep_parameters = keep,
    drop_parameters = drop,
    effects = effects,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
ci.glmm <- function(x, ci = 0.95, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  .ci_generic(model = x, ci = ci, dof = Inf, effects = effects, ...)
}


#' @export
standard_error.glmm <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)

  out <- insight::get_parameters(model, effects = "all")
  out$SE <- sqrt(diag(insight::get_varcov(model, effects = "all")))
  out <- out[, c("Parameter", "SE", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  out
}


#' @export
p_value.glmm <- function(model, effects = c("all", "fixed", "random"), ...) {
  effects <- match.arg(effects)
  s <- summary(model)

  out <- insight::get_parameters(model, effects = "all")
  out$p <- c(s$coefmat[, 4], s$nucoefmat[, 4])
  out <- out[, c("Parameter", "p", "Effects")]

  if (effects != "all") {
    out <- out[out$Effects == effects, , drop = FALSE]
    out$Effects <- NULL
  }

  out
}


#' @export
format_parameters.glmm <- function(model, brackets = c("[", "]"), ...) {
  .format_parameter_default(model, effects = "all", brackets = brackets)
}
