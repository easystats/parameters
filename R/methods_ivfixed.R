
#' @export
ci.ivFixed <- ci.default


#' @export
standard_error.ivFixed <- standard_error.coxr


#' @export
degrees_of_freedom.ivFixed <- function(model, method = "wald", ...) {
  if (is.null(method)) {
    method <- "wald"
  }
  method <- match.arg(tolower(method), choices = c("analytical", "any", "fit", "wald", "residual", "normal"))

  if (method %in% c("wald", "residual", "fit")) {
    as.vector(model$df)
  } else {
    degrees_of_freedom.default(model, method = method, ...)
  }
}


#' @export
p_value.ivFixed <- function(model, method = "wald", ...) {
  stat <- insight::get_statistic(model)
  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(2 * stats::pt(abs(stat$Statistic), df = degrees_of_freedom(model, method = method), lower.tail = FALSE))
    )
  }
}


#' @export
model_parameters.ivFixed <- function(model,
                                     ci = .95,
                                     ci_method = "wald",
                                     verbose = TRUE,
                                     ...) {
  out <- .model_parameters_generic(
    model = model,
    ci = ci,
    ci_method = ci_method,
    merge_by = "Parameter",
    verbose = verbose,
    ...
  )
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  out
}
