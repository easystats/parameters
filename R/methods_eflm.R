# eflm (.eglm) -----------------


#' @export
p_value.eglm <- function(model, ...) {
  stats <- stats::coef(summary(model))
  params <- insight::get_parameters(model)
  .data_frame(
    Parameter = params$Parameter,
    p = as.numeric(as.vector(stats[, 4]))
  )
}
