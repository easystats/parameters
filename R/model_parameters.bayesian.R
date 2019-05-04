#' @rdname model_parameters.stanreg
#' @importFrom insight get_priors
#' @keywords internal
.model_parameters_bayesian <- function(model, ci = .90, ci_method="hdi", standardize = FALSE, estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, diagnostic = TRUE, priors = TRUE, iterations = 1000, ...) {

  # ROPE
  if (all(rope_range == "default")) {
    rope_range <- bayestestR::rope_range(model)
  } else if (!all(is.numeric(rope_range)) | length(rope_range) != 2) {
    stop("`rope_range` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }



  # Processing
  parameters <- .extract_parameters_bayesian(model, ci, ci_method = ci_method, estimate = tolower(estimate), test = test, rope_range = rope_range, iterations = iterations, ...)

  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"full\" by default.")
    standardize <- "full"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    std_parameters <- standardize_parameters(model, method = standardize, estimate = tolower(estimate), ...)
    parameters <- cbind(parameters, std_parameters[names(std_parameters) != "Parameter"])
  }

  # Diagnostic
  if (diagnostic) {
    if (inherits(model, "stanreg")) {
      diagnostic_df <- as.data.frame(model$stan_summary[row.names(model$stan_summary) %in% parameters$Parameter, ])
      parameters$Effective_Sample <- diagnostic_df$n_eff
      parameters$Rhat <- diagnostic_df$Rhat
      # TODO: MCSE
    }
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
#' Parameters of Bayesian models.
#'
#' @param model Bayesian model.
#' @param standardize Add standardized parameters. Can be FALSE or a character indicating the standardization method (see \link{standardize_parameters}).
#' @inheritParams describe_posterior
#' @param priors Include priors specifications information. If set to true (current \code{rstanarm}' default), automatically adjusted priors' scale during fitting  will be displayed.
#' @param diagnostic Include sampling diagnostic metrics (effective sample, Rhat and MCSE). \code{Effective Sample} should be as large as possible, altough for most applications, an effective sample size greater than 1,000 is sufficient for stable estimates (Bürkner, 2017). \code{Rhat} should not be larger than 1.1 (Gelman and Rubin, 1992) or 1.01 (Vehtari et al., 2019).
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootsrapped frequentist models.
#' @param ... Arguments passed to or from other methods (e.g., to \code{standardize}).
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(parameters)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model, standardize=TRUE)
#'
#' library(brms)
#' model <- brms::brm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model)
#' }
#'
#' @references
#' \itemize{
#'  \item{\href{https://easystats.github.io/bayestestR/articles/2_IndicesEstimationComparison.html}{Comparison of Point-Estimates}}
#'  \item{\href{https://easystats.github.io/bayestestR/articles/3_IndicesExistenceComparison.html}{Comparison of Indices of Effect Existence}}
#'  }
#' @importFrom insight get_response
#' @export
model_parameters.stanreg <- .model_parameters_bayesian

#' @export
model_parameters.brmsfit <- .model_parameters_bayesian







#' @importFrom stats sd setNames
#' @keywords internal
.extract_parameters_bayesian <- function(model, ci = .90, ci_method="hdi", estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, priors = TRUE, iterations = 1000, ...) {
  if (insight::model_info(model)$is_bayesian) {
    data <- insight::get_parameters(model)
  } else {
    data <- model_bootstrap(model, iterations = iterations, ...)
  }

  # Summary
  parameters <- describe_posterior(data, ci = ci, ci_method = ci_method, estimate = estimate, test = test, rope_range = rope_range, rope_full = rope_full)
  return(parameters)
}
