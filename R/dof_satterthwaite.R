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


#' @export
dof_satterthwaite.lme <- function(model) {
  insight::check_if_installed("lavaSearch2")

  parameters <- insight::find_parameters(model, effects = "fixed", flatten = TRUE)
  lavaSearch2::sCorrect(model) <- TRUE
  s <- lavaSearch2::summary2(model)
  stats::setNames(as.vector(s$tTable[, "df"]), parameters)
}
