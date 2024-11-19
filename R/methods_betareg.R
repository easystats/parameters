## TODO add ci_method later?

#' @export
model_parameters.betareg <- function(model,
                                     ci = 0.95,
                                     bootstrap = FALSE,
                                     iterations = 1000,
                                     component = "conditional",
                                     standardize = NULL,
                                     exponentiate = FALSE,
                                     p_adjust = NULL,
                                     summary = getOption("parameters_summary", FALSE),
                                     include_info = getOption("parameters_info", FALSE),
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    verbose = verbose
  )

  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  component <- insight::validate_argument(component, c("conditional", "precision", "all"))
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  ## TODO check merge by

  fun_args <- list(
    model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    vcov = NULL,
    vcov_args = NULL
  )
  fun_args <- c(fun_args, dot_args)

  out <- do.call(".model_parameters_generic", fun_args)
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
ci.betareg <- function(x, ci = 0.95, component = "all", verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
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
standard_error.betareg <- function(model, component = "all", verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
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


#' @export
p_value.betareg <- function(model, component = "all", verbose = TRUE, ...) {
  # validation check, warn if unsupported argument is used.
  dot_args <- .check_dots(
    dots = list(...),
    not_allowed = c("vcov", "vcov_args"),
    class(model)[1],
    function_name = "p_value",
    verbose = verbose
  )

  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )

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
simulate_model.betareg <- function(model, iterations = 1000, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "precision")
  )
  out <- .simulate_model(model, iterations, component = component, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}
