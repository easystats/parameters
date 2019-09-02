#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom. If not specified, defaults to model's residual degrees of freedom (i.e. \code{n-k}, where \code{n} is the number of observations and \code{k} is the number of parameters).
#' @inheritParams model_simulate
#'
#' @importFrom stats qt coef
#' @export
ci_wald <- function(model, ci = .95, dof = NULL, component = c("all", "conditional", "zi", "zero_inflated")) {
  component <- match.arg(component)
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, dof = dof, component = component)
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @importFrom insight get_parameters n_obs
#' @importFrom stats qt
#' @keywords internal
.ci_wald <- function(model, ci, dof, component) {
  params <- insight::get_parameters(model, effects = "fixed", component = component)
  estimates <- params$estimate
  se <- standard_error(model, component = component)$SE

  if (is.null(dof)) {
    # residual df
    dof <- insight::n_obs(model) - nrow(params)
  }

  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  out <- cbind(
    CI_low = estimates - se * fac,
    CI_high = estimates + se * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci * 100
  out$Parameter <- params$parameter

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  if ("component" %in% names(params)) out$Component <- params$component

  out
}
