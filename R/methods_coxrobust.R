
#' @export
standard_error.coxr <- function(model, ...) {
  params <- insight::get_parameters(model)
  vc <- insight::get_varcov(model)
  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(sqrt(diag(vc)))
  )
}


#' @export
p_value.coxr <- function(model, ...) {
  stat <- insight::get_statistic(model)
  if (!is.null(stat)) {
    .data_frame(
      Parameter = stat$Parameter,
      p = as.vector(2 * stats::pnorm(abs(stat$Statistic), lower.tail = FALSE))
    )
  }
}
