#' @export
model_parameters.selection <- function(model,
                                       ci = .95,
                                       component = c("all", "selection", "outcome", "auxiliary"),
                                       bootstrap = FALSE,
                                       iterations = 1000,
                                       standardize = NULL,
                                       exponentiate = FALSE,
                                       p_adjust = NULL,
                                       verbose = TRUE,
                                       ...) {
  component <- match.arg(component)
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    component = component,
    merge_by = c("Parameter", "Component"),
    standardize = standardize,
    exponentiate = exponentiate,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
p_value.selection <- function(model, component = c("all", "selection", "outcome", "auxiliary"), ...) {
  component <- match.arg(component)
  s <- summary(model)
  rn <- row.names(s$estimate)
  estimates <- as.data.frame(s$estimate, row.names = FALSE)
  params <- data.frame(
    Parameter = rn,
    p = estimates[[4]],
    Component = "auxiliary",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  params$Component[s$param$index$betaS] <- "selection"
  params$Component[s$param$index$betaO] <- "outcome"

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  text_remove_backticks(params)
}


#' @export
standard_error.selection <- function(model, component = c("all", "selection", "outcome", "auxiliary"), ...) {
  component <- match.arg(component)
  s <- summary(model)
  rn <- row.names(s$estimate)
  estimates <- as.data.frame(s$estimate, row.names = FALSE)
  params <- data.frame(
    Parameter = rn,
    SE = estimates[[2]],
    Component = "auxiliary",
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  params$Component[s$param$index$betaS] <- "selection"
  params$Component[s$param$index$betaO] <- "outcome"

  if (component != "all") {
    params <- params[params$Component == component, , drop = FALSE]
  }

  text_remove_backticks(params)
}



#' @export
simulate_model.selection <- function(model, iterations = 1000, component = c("all", "selection", "outcome", "auxiliary"), ...) {
  component <- match.arg(component)
  out <- .simulate_model(model, iterations, component = component, effects = "fixed")

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- .safe_deparse(substitute(model))
  out
}


#' @export
ci.selection <- ci.default


#' @export
degrees_of_freedom.selection <- function(model, ...) {
  s <- summary(model)
  s$param$df
}
