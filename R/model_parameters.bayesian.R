#' Bayesian Models Parameters
#'
#' Parameters of Bayesian models.
#'
#' @param model Bayesian model.
#' @inheritParams model_parameters.default
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
#' @importFrom insight get_priors
#' @inheritParams insight::get_parameters
#' @export
model_parameters.stanreg <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, effects = "fixed", ...) {

  # Processing
  parameters <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, iterations = iterations, effects = effects, ...)

  if (effects == "fixed")
    attr(parameters, "pretty_names") <- format_parameters(model)
  else
    parameters <- .add_pretty_names(parameters, model, effects = effects, component = NULL)

  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @rdname model_parameters.stanreg
#' @inheritParams insight::get_parameters
#' @export
model_parameters.brmsfit <- function(model, centrality = "median", dispersion = FALSE, ci = .89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1.0, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), priors = TRUE, iterations = 1000, effects = "fixed", component = "all", ...) {

  # Processing
  parameters <- .extract_parameters_bayesian(model, centrality = centrality, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = bf_prior, diagnostic = diagnostic, priors = priors, iterations = iterations, effects = effects, component = component, ...)

  if (effects == "fixed" && component == "conditional")
    attr(parameters, "pretty_names") <- format_parameters(model)
  else
    parameters <- .add_pretty_names(parameters, model, effects = effects, component = component)

  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.MCMCglmm <- model_parameters.stanreg
