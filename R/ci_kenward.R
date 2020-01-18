#' @rdname p_value_kenward
#' @export
ci_kenward <- function(model, ci = .95) {
  ci(model, ci = ci, method = "kenward")
}
