#' @rdname model_parameters.stanreg
#' @importFrom insight get_priors
#' @keywords internal
.model_parameters_bayesian <- function(model, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), standardize = FALSE, standardize_robust = FALSE, iterations = 1000, ...) {

  # Processing
  parameters <- .extract_parameters_bayesian(model, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, diagnostic = diagnostic, iterations = iterations, ...)

  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"full\" by default.")
    standardize <- "full"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    std_parameters <- standardize_parameters(model, method = standardize, robust = standardize_robust, estimate = tolower(estimate), ...)
    parameters <- cbind(parameters, std_parameters[names(std_parameters) != "Parameter"])
  }

  # Priors
  if (priors) {
    if (inherits(model, "stanreg")) {
      priors_data <- insight::get_priors(model)
      names(priors_data) <- tools::toTitleCase(names(priors_data))
      names(priors_data)[-1] <- paste0("Prior_", names(priors_data)[-1])
      names(priors_data) <- gsub("Prior_Adjusted_scale", "Prior_Scale_adjusted", names(priors_data))
      if ("Prior_Scale_adjusted" %in% names(priors_data)) {
        priors_data$Prior_Scale[!is.na(priors_data$Prior_Scale_adjusted)] <- priors_data$Prior_Scale_adjusted[!is.na(priors_data$Prior_Scale_adjusted)]
        priors_data$Prior_Scale_adjusted <- NULL
      }
      parameters <- merge(parameters, priors_data, by = "Parameter", sort = FALSE, all.x = TRUE)
    }
  }

  return(parameters)
}










#' Bayesian Models Parameters
#'
#' Compute Parameters of Bayesian models.
#'
#' @param model Bayesian model.
#' @inheritParams model_parameters.lm
#' @inheritParams bayestestR::describe_posterior
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(parameters)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model, standardize = TRUE)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model)
#' }
#'
#' @references \href{https://easystats.github.io/bayestestR/reference/describe_posterior.html}{Posterior Description}
#' @importFrom insight get_response
#' @export
model_parameters.stanreg <- .model_parameters_bayesian

#' @export
model_parameters.brmsfit <- model_parameters.stanreg







#' @importFrom stats sd setNames
#' @keywords internal
.extract_parameters_bayesian <- function(model, estimate = "median", dispersion = FALSE, ci = .90, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, bf_prior = NULL, diagnostic = c("ESS", "Rhat"), iterations = 1000, ...) {
  if (insight::model_info(model)$is_bayesian) {
    parameters <- bayestestR::describe_posterior(model, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, diagnostic = diagnostic, ...)
  } else {
    data <- model_bootstrap(model, iterations = iterations, ...)
    parameters <- bayestestR::describe_posterior(data, estimate = estimate, dispersion = dispersion, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_full = rope_full, bf_prior = bf_prior, rope_full = rope_full, ...)
  }

  # Summary

  return(parameters)
}
