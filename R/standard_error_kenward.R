#' @rdname p_value_kenward
#' @export
se_kenward <- function(model, ...) {
  UseMethod("se_kenward")
}


#' @export
se_kenward.default <- function(model, ...) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  vcov_adjusted <- insight::get_varcov(model, vcov = "kenward-roger")
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(Parameter = params$Parameter, SE = abs(sqrt(diag(vcov_adjusted))))
}


#' @export
se_kenward.glmmTMB <- function(model, component = "conditional", ...) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  vcov_adjusted <- insight::get_varcov(model, vcov = "kenward-roger")
  params <- insight::get_parameters(model, effects = "fixed", component = component)

  .data_frame(
    Parameter = params$Parameter,
    SE = abs(sqrt(diag(vcov_adjusted))),
    Component = component
  )
}
