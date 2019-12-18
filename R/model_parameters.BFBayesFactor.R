#' BayesFactor objects Parameters
#'
#' Parameters of BayesFactor objects.
#'
#' @param model Object of class \code{BFBayesFactor}.
#' @inheritParams bayestestR::describe_posterior
#'
#'
#' @examples
#' \donttest{
#' library(BayesFactor)
#' model <- ttestBF(x = rnorm(100, 1, 1))
#' model_parameters(model)
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @importFrom stats na.omit
#' @importFrom bayestestR bayesfactor_models
#' @export
model_parameters.BFBayesFactor <- function(model, centrality = "median", dispersion = FALSE, ci = 0.89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 0.89, priors = TRUE, ...) {
  out <- bayestestR::describe_posterior(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, priors = priors, ...)

  # Add components and effects columns
  tryCatch({
    params <- insight::clean_parameters(model)[, c("Parameter", "Effects", "Component")]
    out <- merge(out, params, sort = FALSE)
  }  ,
  error = function(e) { NULL }
  )

  # Extract BF
  tryCatch({
    out$BF <- as.data.frame(bayestestR::bayesfactor_models(model)[-1, ])$BF
  },
  error = function(e) { NULL }
  )

  # Remove unecessary columns
  if ("CI" %in% names(out) && length(stats::na.omit(unique(out$CI))) == 1) {
    out$CI <- NULL
  }
  if ("ROPE_CI" %in% names(out) && length(stats::na.omit(unique(out$ROPE_CI))) == 1) {
    out$ROPE_CI <- NULL
  }
  if ("ROPE_low" %in% names(out)) {
    out$ROPE_low <- NULL
    out$ROPE_high <- NULL
  }

  attr(out, "ci") <- ci
  attr(out, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(out) <- c("parameters_model", class(out))

  out
}
