#' @export
standard_error.lm <- standard_error.glm


#' @export
p_value.lm <- p_value.default


#' @export
ci.lm <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, robust = robust, ...)
}
