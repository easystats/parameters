#' @export
standard_error.lm <- function(model, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"

  if (isTRUE(robust)) {
    standard_error_robust(model, ...)
  } else {
    se <- .get_se_from_summary(model)
    .data_frame(
      Parameter = names(se),
      SE = as.vector(se)
    )
  }
}


#' @include p_value.R
#' @export
p_value.lm <- p_value.default


#' @method ci lm
#' @export
ci.lm <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, robust = robust, ...)
}
