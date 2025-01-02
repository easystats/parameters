# model parameters ---------------------

#' @export
model_parameters.logitor <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     standardize = NULL,
                                     exponentiate = TRUE,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
  model_parameters.default(
    model$fit,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )
}


#' @export
model_parameters.poissonirr <- model_parameters.logitor


#' @export
model_parameters.negbinirr <- model_parameters.logitor


#' @export
model_parameters.poissonmfx <- function(model,
                                        ci = 0.95,
                                        bootstrap = FALSE,
                                        iterations = 1000,
                                        component = "all",
                                        standardize = NULL,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        keep = NULL,
                                        drop = NULL,
                                        verbose = TRUE,
                                        ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "marginal")
  )
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
model_parameters.logitmfx <- model_parameters.poissonmfx


#' @export
model_parameters.probitmfx <- model_parameters.poissonmfx


#' @export
model_parameters.negbinmfx <- model_parameters.poissonmfx


#' @export
model_parameters.betaor <- function(model,
                                    ci = 0.95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = "conditional",
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- insight::validate_argument(
    component,
    c("conditional", "precision", "all")
  )
  model_parameters.betareg(
    model$fit,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = component,
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )
}


#' @export
model_parameters.betamfx <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     component = "all",
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision", "marginal")
  )
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    component = component,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


# ci ------------------

#' @export
ci.logitor <- function(x, ci = 0.95, method = NULL, ...) {
  .ci_generic(model = x$fit, ci = ci, method = method, ...)
}


#' @export
ci.poissonirr <- ci.logitor


#' @export
ci.negbinirr <- ci.logitor


#' @export
ci.poissonmfx <- function(x, ci = 0.95, component = "all", method = NULL, ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "marginal")
  )
  .ci_generic(model = x, ci = ci, component = component, method = method, ...)
}


#' @export
ci.negbinmfx <- ci.poissonmfx


#' @export
ci.logitmfx <- ci.poissonmfx


#' @export
ci.probitmfx <- ci.poissonmfx


#' @export
ci.betaor <- function(x, ci = 0.95, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )
  .ci_generic(model = x$fit, ci = ci, dof = Inf, component = component)
}


#' @export
ci.betamfx <- function(x,
                       ci = 0.95,
                       method = NULL,
                       component = "all",
                       ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision", "marginal")
  )
  .ci_generic(model = x, ci = ci, component = component, method = method, ...)
}


# standard error ------------------

#' @export
standard_error.negbin <- standard_error.default


#' @export
standard_error.logitor <- function(model, ...) {
  standard_error.default(model$fit, ...)
}


#' @export
standard_error.poissonirr <- standard_error.logitor


#' @export
standard_error.negbinirr <- standard_error.logitor


#' @export
standard_error.poissonmfx <- function(model, component = "all", ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "marginal")
  )
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
standard_error.logitmfx <- standard_error.poissonmfx


#' @export
standard_error.probitmfx <- standard_error.poissonmfx


#' @export
standard_error.negbinmfx <- standard_error.poissonmfx


#' @export
standard_error.betaor <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )
  standard_error.betareg(model$fit, component = component, ...)
}


#' @export
standard_error.betamfx <- function(model, component = "all", ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  se <- c(as.vector(model$mfxest[, 2]), as.vector(cs[, 2]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    SE = se,
    Component = parms$Component
  )

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision", "marginal")
  )
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


# p values ------------------

#' @export
p_value.poissonmfx <- function(model, component = "all", ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- stats::coef(summary(model$fit))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "marginal")
  )
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
p_value.logitor <- function(model, method = NULL, ...) {
  p_value.default(model$fit, method = method, ...)
}


#' @export
p_value.poissonirr <- p_value.logitor


#' @export
p_value.negbinirr <- p_value.logitor


#' @export
p_value.logitmfx <- p_value.poissonmfx


#' @export
p_value.probitmfx <- p_value.poissonmfx


#' @export
p_value.negbinmfx <- p_value.poissonmfx


#' @export
p_value.betaor <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )
  p_value.betareg(model$fit, component = component, ...)
}


#' @export
p_value.betamfx <- function(model, component = "all", ...) {
  parms <- insight::get_parameters(model, component = "all")
  cs <- do.call(rbind, stats::coef(summary(model$fit)))
  p <- c(as.vector(model$mfxest[, 4]), as.vector(cs[, 4]))

  out <- .data_frame(
    Parameter = parms$Parameter,
    p = p,
    Component = parms$Component
  )

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision", "marginal")
  )
  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


# simulate model ------------------

#' @export
simulate_model.betaor <- function(model, iterations = 1000, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )

  simulate_model.betareg(model$fit,
    iterations = iterations,
    component = component,
    ...
  )
}


#' @export
simulate_model.betamfx <- simulate_model.betaor
