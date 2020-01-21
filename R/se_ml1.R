#' @rdname p_value_ml1
#' @importFrom stats qnorm
#' @importFrom insight get_parameters
#' @export
se_ml1 <- function(model) {
  params <- insight::get_parameters(model)
  p <- p_value_ml1(model)
  statistic <- stats::qnorm(p$p / 2, lower.tail = FALSE)

  data.frame(
    Parameter = params$Parameter,
    SE = abs(as.vector(params$Estimate / statistic)),
    stringsAsFactors = FALSE
  )
}
