#' @rdname p_value_ml1
#' @export
ci_ml1 <- function(model, ci = .95) {
  ci(model, ci = ci, method = "ml1")
}
