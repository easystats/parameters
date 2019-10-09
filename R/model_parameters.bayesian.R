#' @importFrom insight get_priors
#' @keywords internal
.model_parameters_bayesian <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, ...) {

  # Processing
  parameters <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, iterations = iterations, ...)

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}





#' Bayesian Models Parameters
#'
#' Parameters of Bayesian models.
#'
#' @param model Bayesian model.
#' @inheritParams model_parameters.lm
#' @inheritParams bayestestR::describe_posterior
#'
#' @seealso \code{\link[parameters:standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' \donttest{
#' library(parameters)
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(Sepal.Length ~ Petal.Length * Species,
#'   data = iris, iter = 500, refresh = 0
#' )
#'
#' model_parameters(model)
#' }
#'
#' @return A data.frame of indices related to the model's parameters.
#' @export
model_parameters.stanreg <- .model_parameters_bayesian


#' @export
model_parameters.brmsfit <- model_parameters.stanreg

#' @export
model_parameters.MCMCglmm <- model_parameters.stanreg
