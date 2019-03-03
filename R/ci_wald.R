#' Confidence Interval based on Wald-test approximation
#'
#' @param model A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom.
#'
#'
#' @importFrom stats qt
#' @export
ci_wald <- function(model, ci=0.95, dof=Inf) {
  stopifnot(length(ci) == 1,
            is.numeric(ci),
            ci > 0, ci < 1)

  params <- as.data.frame(stats::coef(summary(model)))

  # all(se > 0))
  alpha <- (1 - ci)/2
  fac <- qt(alpha, df=dof, lower.tail = FALSE)
  out <- cbind(CI_low = params$Estimate - params$Estimate * fac,
               CI_high = params$Estimate + params$Estimate * fac)
  row.names(out) <- row.names(params)
  out
}