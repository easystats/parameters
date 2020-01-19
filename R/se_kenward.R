#' @rdname p_value_kenward
#' @export
se_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  vcov_adj <- pbkrtest::vcovAdj(model)
  params <- insight::get_parameters(model, effects = "fixed")

  data.frame(
    Parameter = params$Parameter,
    SE = abs(as.vector(sqrt(diag(as.matrix(vcov_adj))))),
    stringsAsFactors = FALSE
  )
}
