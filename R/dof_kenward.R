#' @rdname p_value_kenward
#' @export
dof_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  parameters <- find_parameters(model, effects = "fixed", flatten = TRUE)

  L <- as.data.frame(diag(rep(1, n_parameters(model, effects = "fixed"))))
  stats::setNames(sapply(L, pbkrtest::get_ddf_Lb, object = model), parameters)
}
