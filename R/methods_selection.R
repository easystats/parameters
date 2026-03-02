#' @export
model_parameters.selection <- function(model,
                                       ci = 0.95,
                                       component = "all",
                                       bootstrap = FALSE,
                                       iterations = 1000,
                                       standardize = NULL,
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       include_info = getOption("parameters_info", FALSE),
                                       keep = NULL,
                                       drop = NULL,
                                       verbose = TRUE,
                                       ...) {
  component <- insight::validate_argument(
    component,
    c("all", "selection", "outcome", "auxiliary")
  )

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = component,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    keep_parameters = keep,
    drop_parameters = drop,
    include_info = include_info,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
p_value.selection <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "selection", "outcome", "auxiliary")
  )
  s <- summary(model)
  rn <- row.names(s$estimate)
  estimates <- as.data.frame(s$estimate, row.names = FALSE)
  params <- data.frame(
    Parameter = rn,
    p = estimates[[4]],
    Component = "selection",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  params$Component[s$param$index$errTerms] <- "auxiliary"
  params$Component[s$param$index$outcome] <- "outcome"

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  insight::text_remove_backticks(params, verbose = FALSE)
}


#' @export
standard_error.selection <- function(model, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "selection", "outcome", "auxiliary")
  )
  s <- summary(model)
  rn <- row.names(s$estimate)
  estimates <- as.data.frame(s$estimate, row.names = FALSE)
  params <- data.frame(
    Parameter = rn,
    SE = estimates[[2]],
    Component = "selection",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  params$Component[s$param$index$errTerms] <- "auxiliary"
  params$Component[s$param$index$outcome] <- "outcome"

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  insight::text_remove_backticks(params, verbose = FALSE)
}


#' @export
simulate_model.selection <- function(model,
                                     iterations = 1000,
                                     component = "all",
                                     ...) {
  component <- insight::validate_argument(
    component,
    c("all", "selection", "outcome", "auxiliary")
  )
  out <- .simulate_model(model, iterations, component = component, effects = "fixed", ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
ci.selection <- ci.default
