#' p-values using Wald-test approximation
#'
#' The Wald-test approximation treats t-values as Wald z. Since the t distribution converges to the z distribution as degrees of freedom increase, this is like assuming infinite degrees of freedom. While this is unambiguously anti-conservative, this approximation appears as reasonable for reasonable sample sizes (Barr et al., 2013). That is, if we take the p-value to measure the probability of a false positive, this approximation produces a higher false positive rate than the nominal 5% at p = 0.05.
#'
#' @param model A statistical model.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lmer(Petal.Length ~ Sepal.Length + (1 | Species), data = iris)
#' p_value_wald(model)
#' }
#' @importFrom stats coef pnorm
#' @export
p_value_wald <- function(model) {
  UseMethod("p_value_wald")
}


#' @export
p_value_wald.merMod <- function(model) {
  params <- as.data.frame(stats::coef(summary(model)))

  if ("t value" %in% names(params)) {
    p <- 2 * stats::pnorm(abs(params[, "t value"]), lower.tail = FALSE)
  } else if ("z value" %in% names(params)) {
    p <- 2 * stats::pnorm(abs(params[, "z value"]), lower.tail = FALSE)
  } else {
    stop("Couldn't find any suitable statistic (t or z value) for Wald-test approximation.")
  }

  if (is.null(names(p))) {
    coef_names <- rownames(params)
  } else {
    coef_names <- names(p)
  }

  data.frame(
    Parameter = coef_names,
    p = unname(p),
    stringsAsFactors = FALSE
  )
}
