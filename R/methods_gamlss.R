#' @export
ci.gamlss <- function(x, ci = .95, method = NULL, ...) {
  robust <- !is.null(method) && method == "robust"
  ci_wald(model = x, ci = ci, dof = Inf, robust = robust, ...)
}

#' @include methods_gam.R
#' @export
model_parameters.gamlss <- model_parameters.gam
