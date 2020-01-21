#' @rdname p_value_satterthwaite
#' @importFrom stats qnorm
#' @importFrom insight get_parameters
#' @export
se_satterthwaite <- function(model) {
  standard_error(model)
}
