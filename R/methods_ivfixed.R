
#' @export
ci.ivFixed <- ci.default


#' @export
standard_error.ivFixed <- standard_error.coxr


#' @export
degrees_of_freedom.ivFixed <- function(model, ...) {
  as.vector(model$df)
}


#' @export
p_value.ivFixed <- function(model, ...) {
  stat <- insight::get_statistic(model)
  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(2 * stats::pt(abs(stat$Statistic), df = degrees_of_freedom(model), lower.tail = FALSE))
    )
  }
}


#' @export
model_parameters.ivFixed <- function(model,
                                     ci = .95,
                                     verbose = TRUE,
                                     ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    merge_by = "Parameter",
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
