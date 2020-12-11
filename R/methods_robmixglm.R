
#' @importFrom stats na.omit
#' @export
standard_error.robmixglm <- function(model, ...) {
  se <- stats::na.omit(.get_se_from_summary(model))
  .data_frame(
    Parameter = names(se),
    SE = as.vector(se)
  )
}


#' @importFrom stats na.omit
#' @export
p_value.robmixglm <- function(model, ...) {
  p <- stats::na.omit(.get_pval_from_summary(model))
  .data_frame(
    Parameter = names(p),
    p = as.vector(p)
  )
}
