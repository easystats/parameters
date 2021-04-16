#' @rdname p_value_betwithin
#' @export
se_betwithin <- function(model) {
  params <- insight::get_parameters(model)
  p <- p_value_betwithin(model)
  df <- degrees_of_freedom(model, method = "any")
  statistic <- stats::qt(p$p / 2, df = df, lower.tail = FALSE)

  .data_frame(
    Parameter = params$Parameter,
    SE = abs(as.vector(params$Estimate / statistic))
  )
}
