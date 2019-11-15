#' @rdname p_value_wald
#'
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param dof Degrees of Freedom. If not specified, for \code{ci_wald()}, defaults to model's residual degrees of freedom (i.e. \code{n-k}, where \code{n} is the number of observations and \code{k} is the number of parameters). For \code{p_value_wald()}, defaults to \code{Inf}.
#' @inheritParams model_simulate
#' @inheritParams standard_error
#'
#' @importFrom stats qt coef
#' @export
ci_wald <- function(model, ci = .95, dof = NULL, component = c("all", "conditional", "zi", "zero_inflated"), robust = FALSE, ...) {
  component <- match.arg(component)
  out <- lapply(ci, function(i) {
    .ci_wald(model = model, ci = i, dof = dof, component = component, robust = robust, ...)
  })
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}


#' @importFrom insight get_parameters n_obs
#' @importFrom stats qt
#' @keywords internal
.ci_wald <- function(model, ci, dof, component, robust = FALSE, ...) {
  params <- insight::get_parameters(model, effects = "fixed", component = component)
  estimates <- params$Estimate

  stderror <- if (isTRUE(robust)) {
    standard_error_robust(model, ...)
  } else {
    standard_error(model, component = component)
  }

  # filter non-matching parameters
  stderror <- stderror[1:nrow(params), ]
  se <- stderror$SE

  if (is.null(dof)) {
    # residual df
    dof <- degrees_of_freedom(model, method = "any")
    # make sure we have a value for degrees of freedom
    if (is.null(dof) || length(dof) == 0) dof <- Inf
  }

  alpha <- (1 + ci) / 2
  fac <- stats::qt(alpha, df = dof)
  out <- cbind(
    CI_low = estimates - se * fac,
    CI_high = estimates + se * fac
  )

  out <- as.data.frame(out)
  out$CI <- ci * 100
  out$Parameter <- params$Parameter

  out <- out[c("Parameter", "CI", "CI_low", "CI_high")]
  if ("Component" %in% names(params)) out$Component <- params$Component

  out
}
