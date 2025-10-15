#' @rdname p_value_kenward
#' @export
se_kenward <- function(model) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  vcov_adjusted <- insight::get_varcov(model, vcov = "kenward-roger")
  params <- insight::get_parameters(model, effects = "fixed", component)

  .data_frame(Parameter = params$Parameter, SE = abs(sqrt(diag(vcov_adjusted))))
}
