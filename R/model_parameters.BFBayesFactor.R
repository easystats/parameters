#' BayesFactor objects Parameters
#'
#' Parameters of BayesFactor objects.
#'
#' @param model Object of class \code{BFBayesFactor}.
#' @inheritParams bayestestR::describe_posterior
#'
#'
#' @examples
#' \dontrun{
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' model_parameters(bf)
#' }
#'
#' @export
model_parameters.BFBayesFactor <- function(model, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 0.89, priors = TRUE, ...) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("Package 'BayesFactor' needed to plot ROPE. Please install it.")
  }

  out <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, priors = priors, ...)

  # Extract BF
  out$BF <- as.data.frame(bayestestR::bayesfactor_models(model)[-1, ])$BF

  # Remove unecessary columns
  if ("CI" %in% names(out) && length(unique(out$CI)) == 1) {
    out$CI <- NULL
  }
  if ("ROPE_CI" %in% names(out) && length(unique(out$ROPE_CI)) == 1) {
    out$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(out)) {
    out$ROPE_low <- NULL
    out$ROPE_high <- NULL
  }

  out
}
