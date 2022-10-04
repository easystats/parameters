## TODO add ci_method later?

#' @rdname model_parameters.averaging
#' @export
model_parameters.betareg <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     component = c("conditional", "precision", "all"),
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     verbose = TRUE,
                                     ...) {
  # sanity check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    verbose = verbose
  )

  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  args <- list(
    model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    vcov = NULL,
    vcov_args = NULL
  )
  args <- c(args, dot_args)

  out <- do.call(".model_parameters_generic", args)
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
ci.betareg <- function(x,
                       ci = 0.95,
                       component = "all",
                       verbose = TRUE,
                       ...) {
  # sanity check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(x)[1],
    function_name = "ci",
    verbose = verbose
  )

  component <- match.arg(component, choices = c("all", "conditional", "precision"))
  .ci_generic(model = x, ci = ci, dof = Inf, component = component, verbose = verbose)
}


#' @export
standard_error.betareg <- function(model,
                                   component = "all",
                                   verbose = TRUE,
                                   ...) {
  # sanity check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "standard_error",
    verbose = verbose
  )

  component <- match.arg(component, choices = c("all", "conditional", "precision"))

  params <- insight::get_parameters(model)
  cs <- do.call(rbind, stats::coef(summary(model)))
  se <- cs[, 2]

  out <- .data_frame(
    Parameter = .remove_backticks_from_string(names(se)),
    Component = params$Component,
    SE = as.vector(se)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @rdname p_value.DirichletRegModel
#' @export
p_value.betareg <- function(model,
                            component = c("all", "conditional", "precision"),
                            verbose = TRUE,
                            ...) {
  # sanity check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "p_value",
    verbose = verbose
  )

  component <- match.arg(component)

  params <- insight::get_parameters(model)
  cs <- do.call(rbind, stats::coef(summary(model)))
  p <- cs[, 4]

  out <- .data_frame(
    Parameter = params$Parameter,
    Component = params$Component,
    p = as.vector(p)
  )

  if (component != "all") {
    out <- out[out$Component == component, ]
  }

  out
}


#' @export
simulate_model.betareg <- function(model,
                                   iterations = 1000,
                                   component = c("all", "conditional", "precision"),
                                   ...) {
  component <- match.arg(component)
  out <- .simulate_model(model, iterations, component = component, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse(substitute(model))
  out
}
