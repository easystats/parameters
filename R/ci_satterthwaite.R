#' @rdname p_value_satterthwaite
#' @export
ci_satterthwaite <- function(model, ci = .95) {
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, dof = Inf, effect = "fixed", component = "all", method = "satterthwaite")
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
