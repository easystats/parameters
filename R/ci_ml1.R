#' @rdname p_value_ml1
#' @export
ci_ml1 <- function(model, ci = .95, ...) {
  df_ml1 <- dof_ml1(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      effects = "fixed",
      component = "all",
      dof = df_ml1,
      method = "ml1",
      ...
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
