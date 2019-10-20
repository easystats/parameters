#' @rdname p_value_kenward
#' @export
dof_kenward <- function(model) {
  if (!requireNamespace("pbkrtest", quietly = TRUE)) {
    stop("Package `pbkrtest` required for Kenward-Rogers approximation.", call. = FALSE)
  }

  L <- as.data.frame(diag(rep(1, n_parameters(model, effects = "fixed"))))

  ## TODO change to "$Estimate" once fixed in insight
  sapply(L, pbkrtest::get_ddf_Lb, object = model)
}
