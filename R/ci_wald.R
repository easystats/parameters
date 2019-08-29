#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom. If not specified, defaults to model's residual degrees of freedom (i.e. \code{n-k}, where \code{n} is the number of observations and \code{k} is the number of parameters).
#'
#'
#' @importFrom stats qt coef
#' @export
ci_wald <- function(model, ci = .95, dof = NULL) {
  out <- lapply(ci, function(ci, model, dof) .ci_wald(model, ci, dof), model = model, dof = dof)
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @importFrom insight get_parameters n_obs
#' @importFrom stats qt
#' @keywords internal
.ci_wald <- function(x, ci, dof) {
  params <- insight::get_parameters(x, effects = "fixed", component = "conditional")
  estimates <- params$estimate
  se <- standard_error(x)$SE

  if (is.null(dof)) {
    dof <- insight::n_obs(x) - nrow(params)
  }

  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  out <- cbind(
    CI_low = estimates - se * fac,
    CI_high = estimates + se * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci
  out$Parameter <- params$parameter
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  out
}



#' @importFrom insight get_parameters
#' @importFrom stats confint
.ci_profiled_wald <- function(x, ci) {
  out <- as.data.frame(stats::confint(x, level = ci), stringsAsFactors = FALSE)
  names(out) <- c("CI_low", "CI_high")

  out$CI <- ci * 100
  out$Parameter <- insight::get_parameters(x, effects = "fixed", component = "conditional")$parameter

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  rownames(out) <- NULL

  out
}
