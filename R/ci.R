#' @importFrom bayestestR ci
#' @export
bayestestR::ci





#' Confidence Interval (CI)
#'
#' Compute confidence intervals (CI) for frequentist models.
#'
#' @param x A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param method For mixed models, can be \link[=ci_wald]{"wald"} (default) or "boot" (see \code{lme4::confint.merMod}).
#' @param ... Arguments passed to or from other methods.
#'
#' @return A data.frame containing the CI bounds.
#'
#' @importFrom insight find_parameters
#' @export
ci.merMod <- function(x, ci = 0.95, method = c("wald", "boot"), ...) {
  method <- match.arg(method)

  # Wald approx
  if (method == "wald") {
    out <- ci_wald(x)

    # Bootstrapped CIs
  } else if (method == "boot") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required for this function to work. Please install it by running `install.packages('lme4')`.")
    }

    # Compute
    out <- as.data.frame(lme4::confint.merMod(x, level = ci, method = "boot", ...))
    out <- out[rownames(out) %in% insight::find_parameters(x)$conditional, ]
    names(out) <- c("CI_low", "CI_high")

    # Clean up
    out$Parameter <- row.names(out)
    out <- out[c("Parameter", "CI_low", "CI_high")]
    row.names(out) <- NULL
  }

  out
}


#' @rdname ci.merMod
#' @method ci glm
#' @importFrom stats confint
#' @export
ci.glm <- function(x, ci = 0.95, ...) {
  suppressMessages(out <- stats::confint(x, level = ci, ...))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- c("CI_low", "CI_high")

  # Clean up
  out$Parameter <- row.names(out)
  out <- out[c("Parameter", "CI_low", "CI_high")]
  row.names(out) <- NULL
  out
}



#' @method ci lm
#' @export
ci.lm <- ci.glm
