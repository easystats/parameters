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
