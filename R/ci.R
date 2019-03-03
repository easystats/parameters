#' Compute Confidence Intervals
#'
#' This function attempts to return, or compute, p-values of a model's parameters. The nature of the p-values is different depending on the model:
#' \itemize{
#' \item Mixed models (lme4): TO BE IMPROVED.
#' }
#'
#' @param model A statistical model.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%) for frequentist models and 0.90 (90\%) for Bayesian models.
#' @param method For mixed models, can be \link[=ci_wald]{"wald"} (default) or "boot" (see \code{lme4::confint.merMod}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(Petal.Length ~ Sepal.Length + (1|Species), data=iris)
#' ci(model)
#' }
#' @export
ci <- function(model, ci=0.95, ...){
  UseMethod("ci")
}

#' @rdname ci
#' @export
ci.merMod <- function(model, ci=0.95, method = c("wald", "boot"), ...){
  method <- match.arg(method)

  if (method == "wald") {
    out <- ci_wald(model)
  } else if (method == "boot") {
    if (!requireNamespace("lme4", quietly = TRUE))
      stop("Package `lme4` required for bootstrapped approximation of confidence intervals.", call. = FALSE)
    out <- as.data.frame(lme4::confint.merMod(model, level = ci, method = "boot", ...))
    out <- out[rownames(out) %in% insight::find_parameters(model)$conditional, ]
    names(out) <- c("CI_low", "CI_high")
  }

  out
}

#' @export
ci.stanreg <- function(model, ci = 0.95, ...){
  bayestestR::hdi(model, ci = ci, ...)
}

#' @export
ci.brmsfit <- ci.stanreg
