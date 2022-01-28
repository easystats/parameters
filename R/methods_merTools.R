#' @export
model_parameters.merModList <- function(model,
                                        ci = .95,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        verbose = TRUE,
                                        ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = FALSE,
    iterations = 10,
    merge_by = "Parameter",
    standardize = NULL,
    exponentiate = exponentiate,
    robust = FALSE,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}


#' @export
ci.merModList <- function(x, ci = .95, ...) {
  .ci_generic(model = x, ci = ci, dof = NULL, robust = FALSE, component = "conditional")
}


#' @export
standard_error.merModList <- function(model, ...) {
  s <- suppressWarnings(summary(model))
  out <- .data_frame(
    Parameter = s$fe$term,
    SE = s$fe$std.error
  )
  insight::text_remove_backticks(out)
}


#' @export
degrees_of_freedom.merModList <- function(model, ...) {
  s <- suppressWarnings(summary(model))
  s$fe$df
}


#' @export
format_parameters.merModList <- function(model, brackets = c("[", "]"), ...) {
  .format_parameter_default(model[[1]], brackets = brackets)
}
