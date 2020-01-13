#' @rdname p_value_satterthwaite
#' @export
dof_satterthwaite <- function(model) {
  if (!requireNamespace("lmerTest", quietly = TRUE)) {
    stop("Package `lmerTest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  parameters <- find_parameters(model, effects = "fixed", flatten = TRUE)
  lmerTest_model <- lmerTest::as_lmerModLmerTest(model)
  s <- summary(lmerTest_model)

  stats::setNames(as.vector(s$coefficients[, 3]), parameters)
}
