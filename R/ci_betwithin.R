#' @rdname p_value_betwithin
#' @export
ci_betwithin <- function(model, ci = .95) {
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, effects = "fixed", component = "all", dof = Inf, method = "betwithin")
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
