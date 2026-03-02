#' @export
ci.ivFixed <- ci.default


#' @export
standard_error.ivFixed <- standard_error.coxr


#' @export
p_value.ivFixed <- function(model, method = "wald", ...) {
  stat <- insight::get_statistic(model)
  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(2 * stats::pt(
        abs(stat$Statistic),
        df = insight::get_df(model, type = method),
        lower.tail = FALSE
      ))
    )
  }
}


#' @export
model_parameters.ivFixed <- function(model,
                                     ci = 0.95,
                                     ci_method = "wald",
                                     keep = NULL,
                                     drop = NULL,
                                     verbose = TRUE,
                                     ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    merge_by = "Parameter",
    keep_parameters = keep,
    drop_parameters = drop,
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  out
}
