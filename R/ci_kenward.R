#' @rdname p_value_kenward
#' @export
ci_kenward <- function(model, ci = .95) {
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, dof = Inf, effects = "fixed", component = "all", method = "kenward")
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
