#' @export
model_parameters.merModList <- function(model,
                                        ci = 0.95,
                                        exponentiate = FALSE,
                                        p_adjust = NULL,
                                        keep = NULL,
                                        drop = NULL,
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
    keep_parameters = keep,
    drop_parameters = drop,
    p_adjust = p_adjust,
    ...
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}


#' @export
ci.merModList <- function(x, ci = 0.95, ...) {
  .ci_generic(model = x, ci = ci, dof = NULL, component = "conditional", ...)
}


#' @export
standard_error.merModList <- function(model, ...) {
  s <- suppressWarnings(summary(model))
  out <- .data_frame(
    Parameter = s$fe$term,
    SE = s$fe$std.error
  )
  insight::text_remove_backticks(out, verbose = FALSE)
}


#' @export
format_parameters.merModList <- function(model, brackets = c("[", "]"), ...) {
  .format_parameter_default(model[[1]], brackets = brackets)
}
