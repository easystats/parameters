#' @export
model_parameters.glmx <- function(model,
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
  component <- insight::validate_argument(component, c("all", "conditional", "extra"))
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = merge_by,
    standardize = standardize,
    exponentiate = exponentiate,
    keep_parameters = keep,
    drop_parameters = drop,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
standard_error.glmx <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = c(as.vector(stats$glm[, "Std. Error"]), as.vector(stats$extra[, "Std. Error"])),
    Component = params$Component
  )
}


#' @export
p_value.glmx <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    p = c(as.vector(stats$glm[, "Pr(>|z|)"]), as.vector(stats$extra[, "Pr(>|z|)"])),
    Component = params$Component
  )
}



#' @export
simulate_model.glmx <- function(model, iterations = 1000, component = "all", ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "extra")
  )
  out <- .simulate_model(model, iterations, component = component, ...)

  class(out) <- c("parameters_simulate_model", class(out))
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}
