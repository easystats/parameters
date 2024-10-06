#' @export
standard_error.biglm <- function(model, ...) {
  cs <- summary(model)$mat
  params <- insight::get_parameters(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(cs[, 4])
  )
}
