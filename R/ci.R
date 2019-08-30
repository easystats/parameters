#' Confidence Interval (CI)
#'
#' Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param method For mixed models of class \code{merMod}, can be \code{\link[=ci_wald]{"wald"}} (default) or \code{"boot"} (see \code{lme4::confint.merMod}). For generalized linear models, can be \code{"profile"} (default) or \code{"wald"}.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams model_simulate
#'
#' @return A data.frame containing the CI bounds.
#'
#' @importFrom insight find_parameters
#' @export
ci.merMod <- function(x, ci = 0.95, method = c("wald", "boot"), ...) {
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(model = x, ci = ci, dof = Inf)

    # Bootstrapped CIs
  } else if (method == "boot") {
    out <- lapply(ci, function(ci, x) .ci_boot_merMod(x, ci, ...), x = x)
    out <- do.call(rbind, out)
    row.names(out) <- NULL
  }
  out
}


#' @importFrom bayestestR ci
#' @export
bayestestR::ci



#' @rdname ci.merMod
#' @method ci glm
#' @export
ci.glm <- function(x, ci = .95, method = c("profile", "wald"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled(model = x, ci = i))
  } else {
    out <- lapply(ci, function(i) ci_wald(model = x, ci = i, component = "conditional"))
  }
  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
}



#' @method ci lm
#' @export
ci.lm <- function(x, ci = .95, ...) {
  ci_wald(model = x, ci = ci, component = "conditional")
}



#' @export
ci.BBmm <- ci.lm



#' @export
ci.BBreg <- ci.lm



#' @rdname ci.merMod
#' @export
ci.glmmTMB <- function(x, ci = .95, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}



#' @export
ci.polr <- function(x, ci = .95, method = c("profile", "wald"), ...) {
  method <- match.arg(method)
  if (method == "profile") {
    out <- lapply(ci, function(i) .ci_profiled2(model = x, ci = i))
  } else {
    out <- lapply(ci, function(i) ci_wald(model = x, ci = i, component = "conditional"))
  }

  out <- do.call(rbind, out)

  # for polr, profiled CI do not return CI for response levels
  # thus, we also calculate Wald CI and add missing rows to result

  out_missing <- ci_wald(model = x, ci = ci, component = "conditional")
  missing_rows <- out_missing$Parameter %in% setdiff(out_missing$Parameter, out$Parameter)
  out <- rbind(out, out_missing[missing_rows, ])

  # fix names, to match standard error and p_value

  out$Parameter <- gsub("Intercept: ", "", out$Parameter, fixed = TRUE)
  row.names(out) <- NULL

  out
}



#' @rdname ci.merMod
#' @export
ci.MixMod <- function(x, ci = .95, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)
  ci_wald(model = x, ci = ci, dof = Inf, component = component)
}
