#' @rdname ci
#' @importFrom bayestestR ci
#' @export
bayestestR::ci







#' @title Confidence/Credible Interval
#'
#' @description Compute the confidence/credible interval of the parameters.
#'
#' @method ci merMod
#' @param x bla
#' @param ci bla
#' @param method For mixed models, can be \link[=ci_wald]{"wald"} (default) or "boot" (see \code{lme4::confint.merMod}).
#' @export
ci.merMod <- function(x, ci = 0.95, method = c("wald", "boot"), ...) {
  method <- match.arg(method)

  if (method == "wald") {
    out <- ci_wald(x)
  } else if (method == "boot") {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package `lme4` required for bootstrapped approximation of confidence intervals.", call. = FALSE)
    }
    out <- as.data.frame(lme4::confint.merMod(x, level = ci, method = "boot", ...))
    out <- out[rownames(out) %in% insight::find_parameters(x)$conditional, ]
    names(out) <- c("CI_low", "CI_high")
  }

  out
}


#' @rdname ci
#' @method ci glm
#' @export
ci.glm <- function(x, ci = 0.95, ...){
  suppressMessages(out <- confint(x, level = ci, ...))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- c("CI_low", "CI_high")
  out
}

#' @rdname ci
#' @method ci lm
#' @export
ci.lm <- ci.glm

