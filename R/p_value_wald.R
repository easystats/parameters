#' Wald-test approximation for CIs and p-values
#'
#' The Wald-test approximation treats t-values as Wald z. Since the t distribution converges to the z distribution as degrees of freedom increase, this is like assuming infinite degrees of freedom. While this is unambiguously anti-conservative, this approximation appears as reasonable for reasonable sample sizes (Barr et al., 2013). That is, if we take the p-value to measure the probability of a false positive, this approximation produces a higher false positive rate than the nominal 5\% at p = 0.05.
#'
#' @param model A statistical model.
#' @param ... Arguments passed down to \code{standard_error_robust()} when confidence intervals or p-values based on robust standard errors should be computed.
#'
#' @examples
#' \donttest{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_wald(model)
#' ci_wald(model, ci = c(0.90, 0.95))
#' }
#'
#' @return A data frame.
#' @importFrom stats coef pt
#' @references Barr, D. J. (2013). Random effects structure for testing interactions in linear mixed-effects models. Frontiers in psychology, 4, 328.
#' @export
p_value_wald <- function(model, ...) {
  UseMethod("p_value_wald")
}


#' @rdname p_value_wald
#' @export
p_value_wald.merMod <- function(model, dof = Inf, ...) {
  params <- as.data.frame(stats::coef(summary(model)))
  .p_value_wald(params, dof)
}


#' @export
p_value_wald.rlmerMod <- function(model, dof = Inf, ...) {
  params <- as.data.frame(stats::coef(summary(model)))
  .p_value_wald(params, dof)
}


#' @export
p_value_wald.cpglmm <- function(model, dof = Inf, ...) {
  if (!requireNamespace("cplm", quietly = TRUE)) {
    stop("To use this function, please install package 'cplm'.")
  }
  params <- as.data.frame(cplm::summary(model)$coefs)
  .p_value_wald(params, dof)
}


.p_value_wald <- function(params, dof = NULL) {
  if (is.null(dof)) dof <- Inf

  if ("t value" %in% names(params)) {
    p <- 2 * stats::pt(abs(params[, "t value"]), df = dof, lower.tail = FALSE)
  } else if ("z value" %in% names(params)) {
    p <- 2 * stats::pt(abs(params[, "z value"]), df = dof, lower.tail = FALSE)
  } else {
    stop("Couldn't find any suitable statistic (t or z value) for Wald-test approximation.")
  }

  if (is.null(names(p))) {
    coef_names <- rownames(params)
  } else {
    coef_names <- names(p)
  }

  .data_frame(
    Parameter = .remove_backticks_from_string(coef_names),
    p = unname(p)
  )
}
