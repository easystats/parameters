#' @export
model_parameters.bfsl <- function(model,
                                  ci = 0.95,
                                  ci_method = "residual",
                                  p_adjust = NULL,
                                  summary = getOption("parameters_summary", FALSE),
                                  include_info = getOption("parameters_info", FALSE),
                                  keep = NULL,
                                  drop = NULL,
                                  verbose = TRUE,
                                  ...) {
  ## TODO remove deprecated later
  if (!missing(summary)) {
    .deprecated_warning("summary", "include_info", verbose)
    include_info <- summary
  }

  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    merge_by = "Parameter",
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
standard_error.bfsl <- function(model, ...) {
  cf <- stats::coef(model)

  params <- data.frame(
    Parameter = rownames(cf),
    SE = unname(cf[, "Std. Error"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  insight::text_remove_backticks(params, verbose = FALSE)
}
