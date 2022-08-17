#' @rdname p_value_satterthwaite
#' @export
se_satterthwaite <- function(model) {
  UseMethod("se_satterthwaite")
}

#' @export
se_satterthwaite.default <- function(model) {
  # check for valid input
  .is_model_valid(model)
  standard_error(model)
}

#' @export
se_satterthwaite.lme <- function(model) {
  insight::check_if_installed("lavaSearch2")
  params <- insight::get_parameters(model, effects = "fixed")
  lavaSearch2::sCorrect(model) <- TRUE
  s <- lavaSearch2::summary2(model)

  .data_frame(
    Parameter = params$Parameter,
    SE = as.vector(s$tTable[, "Std.Error"])
  )
}

#' @export
se_satterthwaite.gls <- se_satterthwaite.lme
