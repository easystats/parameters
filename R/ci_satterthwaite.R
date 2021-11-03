#' @rdname p_value_satterthwaite
#' @export
ci_satterthwaite <- function(model, ci = .95, robust = FALSE, ...) {
  df_satter <- dof_satterthwaite(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_satter,
      effects = "fixed",
      component = "all",
      method = "satterthwaite",
      robust = robust,
      ...
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
