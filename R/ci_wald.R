#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom.
#'
#'
#' @importFrom stats qt coef
#' @export
ci_wald <- function(model, ...) {
  UseMethod("ci_wald")
}


#' @rdname p_value_wald
#' @export
ci_wald.default <- function(model, ci = 0.95, dof = Inf) {
  out <- lapply(ci, function(ci, model, dof) .ci_wald(model, ci, dof), model = model, dof = dof)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @export
ci_wald.wbm <- function(model, ci = 0.95, dof = Inf) {
  out <- lapply(ci, function(ci, model, dof) .ci_wald_panelr(model, ci, dof), model = model, dof = dof)

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @keywords internal
.ci_wald <- function(x, ci, dof = Inf) {
  params <- as.data.frame(stats::coef(summary(x)))

  # all(se > 0))
  alpha <- (1 - ci) / 2
  fac <- stats::qt(alpha, df = dof, lower.tail = FALSE)
  out <- cbind(
    CI_low = params$Estimate - params$`Std. Error` * fac,
    CI_high = params$Estimate + params$`Std. Error` * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci
  out$Parameter <- row.names(params)
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  out
}


#' @keywords internal
.ci_wald_panelr <- function(x, ci, dof = Inf) {
  params <- as.data.frame(x@summ$coeftable, stringsAsFactors = FALSE)

  # all(se > 0))
  alpha <- (1 - ci) / 2
  fac <- stats::qt(alpha, df = dof, lower.tail = FALSE)
  out <- cbind(
    CI_low = params[["Est."]] - params[["S.E."]] * fac,
    CI_high = params[["Est."]] + params[["S.E."]] * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci
  out$Parameter <- row.names(params)
  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  out
}
