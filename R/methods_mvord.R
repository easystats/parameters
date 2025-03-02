# classes: .mvord

#################### .mvord


#' @export
model_parameters.mvord <- function(model,
                                   ci = 0.95,
                                   component = "all",
                                   standardize = NULL,
                                   exponentiate = FALSE,
                                   p_adjust = NULL,
                                   include_info = getOption("parameters_info", FALSE),
                                   keep = NULL,
                                   drop = NULL,
                                   verbose = TRUE,
                                   ...) {
  component <- insight::validate_argument(component, c("all", "conditional", "thresholds", "correlation"))

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = c("Parameter", "Component", "Response"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.mvord <- function(model, component = c("all", "conditional", "thresholds", "correlation"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = "all")
  junk <- utils::capture.output({
    s <- summary(model)
  })

  params$SE <- c(
    unname(s$thresholds[, "Std. Error"]),
    unname(s$coefficients[, "Std. Error"]),
    unname(s$error.structure[, "Std. Error"])
  )

  params <- params[c("Parameter", "SE", "Component", "Response")]

  if (insight::n_unique(params$Response) == 1) {
    params$Response <- NULL
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  insight::text_remove_backticks(params, verbose = FALSE)
}


#' @export
p_value.mvord <- function(model, component = c("all", "conditional", "thresholds", "correlation"), ...) {
  component <- match.arg(component)
  params <- insight::get_parameters(model, component = "all")
  junk <- utils::capture.output({
    s <- summary(model)
  })

  params$p <- c(
    unname(s$thresholds[, "Pr(>|z|)"]),
    unname(s$coefficients[, "Pr(>|z|)"]),
    unname(s$error.structure[, "Pr(>|z|)"])
  )

  params <- params[c("Parameter", "p", "Component", "Response")]

  if (insight::n_unique(params$Response) == 1) {
    params$Response <- NULL
  }

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  insight::text_remove_backticks(params, verbose = FALSE)
}


#' @export
simulate_model.mvord <- function(model, iterations = 1000, component = c("all", "conditional", "thresholds", "correlation"), ...) {
  component <- match.arg(component)
  out <- .simulate_model(model, iterations, component = component, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}
