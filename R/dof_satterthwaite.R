#' @rdname p_value_satterthwaite
#' @export
dof_satterthwaite <- function(model) {
  insight::get_df(model, "satterthwaite")
}
