
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


#' @include methods_lmtest.R
#' @export
model_parameters.ivFixed <- model_parameters.coeftest
