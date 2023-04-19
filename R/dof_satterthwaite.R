#' @rdname p_value_satterthwaite
#' @export
dof_satterthwaite <- function(model) {
  UseMethod("dof_satterthwaite")
}


#' @export
dof_satterthwaite.lmerMod <- function(model) {
  insight::check_if_installed("lmerTest")

  parameters <- insight::find_parameters(model, effects = "fixed", flatten = TRUE)
  lmerTest_model <- lmerTest::as_lmerModLmerTest(model)
  s <- summary(lmerTest_model)

  stats::setNames(as.vector(s$coefficients[, 3]), parameters)
}
