#' @rdname p_value_satterthwaite
#' @importFrom stats qnorm
#' @importFrom insight get_parameters
#' @export
se_satterthwaite <- function(model) {
  params <- insight::get_parameters(model)
  lmerTest_model <- lmerTest::as_lmerModLmerTest(model)
  s <- summary(lmerTest_model)

  data.frame(
    Parameter = params$Parameter,
    SE = as.vector(sqrt(diag(lmerTest_model@vcov_varpar))),
    stringsAsFactors = FALSE
  )
}