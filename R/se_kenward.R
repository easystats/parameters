#' @rdname p_value_kenward
#' @importFrom insight get_parameters
#' @export
se_kenward <- function(model) {
  .check_REML_fit(model)
  vcov_adj <- .vcov_kenward_ajusted(model)
  params <- insight::get_parameters(model, effects = "fixed")

  .data_frame(
    Parameter = params$Parameter,
    SE = abs(as.vector(sqrt(diag(as.matrix(vcov_adj)))))
  )
}
