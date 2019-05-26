#' BayesFactor objects Parameters
#'
#' Parameters of BayesFactor objects.
#'
#' @param model Object of class \code{BFBayesFactor}.
#' @inheritParams bayestestR::describe_posterior
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' \dontrun{
#' library(BayesFactor)
#' bf <- ttestBF(x = rnorm(100, 1, 1))
#' model_parameters(bf)
#' }
#'
#'
#' @export
model_parameters.BFBayesFactor <- function(model,  ...) {
  if (!requireNamespace("BayesFactor", quietly = TRUE)) {
    stop("Package 'BayesFactor' needed to plot ROPE. Please install it.")
  }

  out <- bayestestR::describe_posterior(model, ...)

  # Extract BF
  numerator <- model@numerator[[names(model@numerator)]]
  out$BF <- exp(numerator@analysis$bf)

  # Extract prior
  out$Prior <- numerator@prior$rscale

  out
}
