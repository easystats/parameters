# classes: .glimML


#################### .glimML ------


#' @export
model_parameters.glimML <- function(model,
                                    ci = .95,
                                    bootstrap = FALSE,
                                    iterations = 1000,
                                    component = c("conditional", "random", "dispersion", "all"),
                                    standardize = NULL,
                                    exponentiate = FALSE,
                                    robust = FALSE,
                                    p_adjust = NULL,
                                    verbose = TRUE,
                                    ...) {
  component <- match.arg(component)
  if (component == "all") {
    merge_by <- c("Parameter", "Component")
  } else {
    merge_by <- "Parameter"
  }

  # dispersion is just an alias...
  if (component == "dispersion") {
    component <- "random"
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    component = component,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = "Parameter",
    standardize = standardize,
    exponentiate = exponentiate,
    robust = robust,
    p_adjust = p_adjust,
    verbose = verbose,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
standard_error.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  se <- s[, 2]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    SE = as.vector(se)
  )
}


#' @export
p_value.glimML <- function(model, ...) {
  insight::check_if_installed("aod")

  s <- methods::slot(aod::summary(model), "Coef")
  p <- s[, 4]

  .data_frame(
    Parameter = .remove_backticks_from_string(rownames(s)),
    p = as.vector(p)
  )
}
