#' @inheritParams model_parameters.merMod
#' @export
model_parameters.wbm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, exponentiate = FALSE, summary_random = FALSE, ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    bootstrap = bootstrap,
    iterations = iterations,
    merge_by = c("Parameter", "Component"),
    standardize = NULL,
    exponentiate = exponentiate,
    ...
  )

  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)

  if (isTRUE(summary_random)) {
    attr(parameters, "summary_random") <- .randomeffects_summary(model)
  }

  out
}


#' @export
model_parameters.wbgee <- model_parameters.wbm
