#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom.
#'
#'
#' @importFrom stats qt coef
#' @export
ci_wald <- function(model, ci = 0.95, dof = Inf) {
  stopifnot(
    length(ci) == 1,
    is.numeric(ci),
    ci > 0, ci < 1
  )

  params <- as.data.frame(stats::coef(summary(model)))

  # all(se > 0))
  alpha <- (1 - ci) / 2
  fac <- stats::qt(alpha, df = dof, lower.tail = FALSE)
  out <- cbind(
    CI_low = params$Estimate - params$Estimate * fac,
    CI_high = params$Estimate + params$Estimate * fac
  )

  out <- as.data.frame(out)
  out$Parameter <- row.names(params)
  out <- out[c("Parameter", "CI_low", "CI_high")]
  out
}
