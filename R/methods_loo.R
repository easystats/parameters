#' Bayesian Model Comparison
#'
#' Make a table of Bayesian model comparisons using the `loo` package.
#'
#' @param model An object of class [brms::loo_compare].
#' @param include_IC Whether to include the information criteria (IC).
#' @param include_ENP Whether to include the effective number of parameters (ENP).
#' @param ... Additional arguments (not used for now).
#'
# nolint start
#' @examplesIf requireNamespace("brms", quietly = TRUE) && requireNamespace("RcppEigen", quietly = TRUE) && requireNamespace("BH", quietly = TRUE)
# nolint end
#' \donttest{
#' library(brms)
#'
#' m1 <- brms::brm(mpg ~ qsec, data = mtcars)
#' m2 <- brms::brm(mpg ~ qsec + drat, data = mtcars)
#' m3 <- brms::brm(mpg ~ qsec + drat + wt, data = mtcars)
#'
#' x <- suppressWarnings(brms::loo_compare(
#'   brms::add_criterion(m1, "loo"),
#'   brms::add_criterion(m2, "loo"),
#'   brms::add_criterion(m3, "loo"),
#'   model_names = c("m1", "m2", "m3")
#' ))
#' parameters(x)
#' parameters(x, include_IC = FALSE, include_ENP = TRUE)
#' }
#'
#' @details
#' The rule of thumb is that the models are "very similar" if |elpd_diff| (the
#' absolute value of elpd_diff) is less than 4 (Sivula, Magnusson and Vehtari, 2020).
#' If superior to 4, then one can use the SE to obtain a standardized difference
#' (Z-diff) and interpret it as such, assuming that the difference is normally
#' distributed. The corresponding p-value is then calculated as `2 * pnorm(-abs(Z-diff))`.
#' However, note that if the raw ELPD difference is small (less than 4), it doesn't
#' make much sense to rely on its standardized value: it is not very useful to
#' conclude that a model is much better than another if both models make very
#' similar predictions.
#'
#' @return Objects of `parameters_model`.
#' @export
model_parameters.compare.loo <- function(model, include_IC = TRUE, include_ENP = FALSE, ...) {
  # nolint start
  # https://stats.stackexchange.com/questions/608881/how-to-interpret-elpd-diff-of-bayesian-loo-estimate-in-bayesian-logistic-regress
  # nolint end
  # https://users.aalto.fi/%7Eave/CV-FAQ.html#12_What_is_the_interpretation_of_ELPD__elpd_loo__elpd_diff
  # https://users.aalto.fi/%7Eave/CV-FAQ.html#se_diff

  # The difference in expected log predictive density (elpd) between each model
  # and the best model as well as the standard error of this difference (assuming
  # the difference is approximately normal).

  # The values in the first row are 0s because the models are ordered from best to worst according to their elpd.
  x <- as.data.frame(model)

  out <- data.frame(Name = rownames(x))
  if ("looic" %in% colnames(x)) {
    if(include_IC) out$LOOIC <- x[["looic"]]
    if(include_ENP) out$ENP <- x[["p_loo"]]
    out$ELPD <- x[["elpd_loo"]]
  } else {
    if(include_IC) out$WAIC <- x[["waic"]]
    if(include_ENP) out$ENP <- x[["p_waic"]]
    out$ELPD <- x[["elpd_waic"]]
  }

  out$Difference <- x[["elpd_diff"]]
  out$Difference_SE <- x[["se_diff"]]

  z_elpd_diff <- x[["elpd_diff"]] / x[["se_diff"]]
  out$p <- 2 * stats::pnorm(-abs(z_elpd_diff))

  class(out) <- c("parameters_model", "data.frame")
  out
}