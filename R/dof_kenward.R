#' @rdname p_value_kenward
#' @export
dof_kenward <- function(model) {
  insight::get_df(model, "kenward")
}
