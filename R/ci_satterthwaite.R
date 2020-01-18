#' @rdname p_value_satterthwaite
#' @export
ci_satterthwaite <- function(model, ci = .95) {
  ci(model, ci = ci, method = "satterthwaite")
}
