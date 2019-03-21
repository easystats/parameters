#' @importFrom bayestestR ci
#' @export
bayestestR::ci







#' Confidence Interval
#'
#' @method ci merMod
#' @param method For mixed models, can be \link[=ci_wald]{"wald"} (default) or "boot" (see \code{lme4::confint.merMod}).
#' @export
ci.merMod <- function(model, ci = 0.95, method = c("wald", "boot"), ...) {
  method <- match.arg(method)

  if (method == "wald") {
    out <- ci_wald(model)
  } else if (method == "boot") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` required for bootstrapped approximation of confidence intervals.", call. = FALSE)
    }
    out <- as.data.frame(lme4::confint.merMod(model, level = ci, method = "boot", ...))
    out <- out[rownames(out) %in% insight::find_parameters(model)$conditional, ]
    names(out) <- c("CI_low", "CI_high")
  }

  out
}