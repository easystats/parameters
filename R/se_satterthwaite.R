#' @rdname p_value_satterthwaite
#' @importFrom stats qnorm
#' @importFrom insight get_parameters
#' @export
se_satterthwaite <- function(model) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package `lmerTest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  params <- insight::get_parameters(model)
  lmerTest_model <- lmerTest::as_lmerModLmerTest(model)

  data.frame(
    Parameter = params$Parameter,
    SE = as.vector(sqrt(diag(lmerTest_model@vcov_varpar))),
    stringsAsFactors = FALSE
  )
}