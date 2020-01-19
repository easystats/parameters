#' @rdname p_value_ml1
#' @export
ci_ml1 <- function(model, ci = .95) {
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, effect = "fixed", component = "all", dof = Inf, method = "ml1")
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
