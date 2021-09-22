#' @rdname p_value_kenward
#' @export
ci_kenward <- function(model, ci = .95) {
  .check_REML_fit(model)
  df_kr <- dof_kenward(model)
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_kr,
      effects = "fixed",
      component = "all",
      method = "kenward",
      se = attr(df_kr, "se", exact = TRUE)
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}



.ci_kenward_dof <- function(model, ci = .95, df_kr) {
  out <- lapply(ci, function(i) {
    .ci_dof(
      model = model,
      ci = i,
      dof = df_kr$df_error,
      effects = "fixed",
      component = "all",
      method = "kenward",
      se = df_kr$SE
    )
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}
