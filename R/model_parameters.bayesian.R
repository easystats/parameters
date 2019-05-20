#' @importFrom insight get_priors
#' @keywords internal
.model_parameters_bayesian <- function(model, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, standardize = FALSE, standardize_robust = FALSE, iterations = 1000, ...) {

  # Processing
  parameters <- .extract_parameters_bayesian(model, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, iterations = iterations, ...)

  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"full\" by default.")
    standardize <- "full"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    std_parameters <- standardize_parameters(model, method = standardize, robust = standardize_robust, estimate = tolower(estimate), ...)
    parameters <- cbind(parameters, std_parameters[names(std_parameters) != "Parameter"])
  }


  # Remove unecessary columns
  # CI
  if("CI" %in% names(parameters) && length(unique(parameters$CI)) == 1){
    parameters$CI <- NULL
  }
  if("ROPE_CI" %in% names(parameters) && length(unique(parameters$ROPE_CI)) == 1){
    parameters$ROPE_CI <- NULL
  }
  if("ROPE_low" %in% names(parameters)){
    parameters$ROPE_low <- NULL
    parameters$ROPE_high <- NULL
  }

  return(parameters)
}







#' @importFrom stats sd setNames
#' @keywords internal
.extract_parameters_bayesian <- function(model, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, ...) {

  # Bayesian Models
  if (insight::model_info(model)$is_bayesian) {
    parameters <- bayestestR::describe_posterior(model, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, ...)

    # Bootstrapped Models
  } else {
    data <- model_bootstrap(model, iterations = iterations)
    parameters <- bayestestR::describe_posterior(data, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, ...)
  }

  parameters
}






#' Bayesian Models Parameters
#'
#' Parameters of Bayesian models.
#'
#' @inheritParams model_parameters.lm
#' @inheritParams bayestestR::describe_posterior
#' @param model Bayesian model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Length ~ Species, data=iris)
#' model_parameters(model, standardize = "full")
#'
#' library(brms)
#' model <- brm(Sepal.Length ~ Species, data=iris)
#' model_parameters(model)
#' }
#' @export
model_parameters.stanreg <- .model_parameters_bayesian


#' @export
model_parameters.brmsfit <- model_parameters.stanreg

