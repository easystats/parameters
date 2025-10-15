#' @rdname p_value_kenward
#' @export
se_kenward <- function(model) {
  if (!.check_REML_fit(model)) {
    model <- stats::update(model, . ~ ., REML = TRUE)
  }

  dof <- insight::get_df(model, "kenward")
  params <- insight::get_parameters(model, effects = "fixed", component)

  .data_frame(Parameter = params$Parameter, SE = attributes(dof)$se)
}
