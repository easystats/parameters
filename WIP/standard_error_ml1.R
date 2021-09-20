#' @rdname p_value_ml1
#' @export
se_ml1 <- function(model) {
  params <- insight::get_parameters(model)
  p <- p_value_ml1(model)
  df <- degrees_of_freedom(model, method = "any")
  statistic <- stats::qt(p$p / 2, df = df, lower.tail = FALSE)

  .data_frame(
    Parameter = params$Parameter,
    SE = abs(as.vector(params$Estimate / statistic))
  )
}
