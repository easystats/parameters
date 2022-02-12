#' @rdname p_value_betwithin
#' @export
ci_betwithin <- function(model, ci = .95, ...) {
  df_bet <- dof_ml1(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      effects = "fixed",
      component = "all",
      dof = df_bet,
      method = "betwithin",
      ...
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
