#' @rdname p_value_satterthwaite
#' @export
ci_satterthwaite <- function(model, ci = 0.95, ...) {
  df_satter <- dof_satterthwaite(model)
  if (inherits(model, "glmmTMB")) {
    component <- "conditional"
  } else {
    component <- "all"
  }
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_satter,
      effects = "fixed",
      component = component,
      method = "satterthwaite",
      ...
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
