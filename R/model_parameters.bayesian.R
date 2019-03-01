#' @keywords internal
.model_parameters_bayesian <- function(model, ci = .90, standardize = FALSE, estimate = "median", test = c("pd", "rope"), rope_bounds = "default", rope_full = TRUE, diagnostic = TRUE, priors=TRUE, iterations = 1000, ...) {

  # ROPE
  if (all(rope_bounds == "default")) {
    rope_bounds <- bayestestR::rope_bounds(model)
  } else if (!all(is.numeric(rope_bounds)) | length(rope_bounds) != 2) {
    stop("`rope_bounds` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  # Processing
  parameters <- .extract_parameters_bayesian(model, ci, estimate = tolower(estimate), test = test, rope_bounds = rope_bounds, iterations = iterations, ...)

  # Standardized
  if (standardize) {
    std_model <- standardize(model, ...)
    std_parameters <- .extract_parameters_bayesian(std_model, ci = ci, estimate = tolower(estimate), test = NULL, rope_bounds = rope_bounds, iterations = iterations, ...)
    names(std_parameters) <- paste0("Std_", names(std_parameters))

    parameters <- cbind(parameters, std_parameters[names(std_parameters) != "Std_Parameter"])
  }

  # Diagnostic
  if(diagnostic){
    if(inherits(model, "stanreg")){
      diagnostic_df <- as.data.frame(model$stan_summary[row.names(model$stan_summary) %in% parameters$Parameter, ])
      parameters$Effective_Sample <- diagnostic_df$n_eff
      parameters$Rhat <- diagnostic_df$Rhat
      # TODO: MCSE
    }
  }

  # Priors
  if(priors){
    if(inherits(model, "stanreg")){
      priors_data <- get_priors(model)
      if("Prior_Scale_adjusted" %in% names(priors_data)){
        priors_data$Prior_Scale <- priors_data$Prior_Scale_adjusted
        priors_data$Prior_Scale_adjusted <- NULL
      }
      parameters <- merge(parameters, priors_data, by="Parameter", sort=FALSE, all.x = TRUE)
    }
  }

  return(parameters)
}










#' Bayesian Models Parameters
#'
#' Parameters of Bayesian models.
#'
#' @param model Bayesian model.
#' @param standardize Add standardized parameters. Default to FALSE as this re-fits the model and can thus take some time.
#' @inheritParams summarise_posteriors
#' @param priors Include priors specifications information. If set to true (current \code{rstanarm}' default), automatically adjusted priors' scale during fitting  will be displayed.
#' @param diagnostic Include sampling diagnostic metrics (effective sample, Rhat and MCSE). \code{Effective Sample} should be as large as possible, altough for most applications, an effective sample size greater than 1,000 is sufficient for stable estimates (Bürkner, 2017). \code{Rhat} should not be larger than 1.1.
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootsrapped frequentist models.
#' @param ... Arguments passed to or from other methods (e.g., to \code{standardize}).
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model)
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
.extract_parameters_bayesian <- function(model, ci = .90, estimate = "median", test = c("pd", "rope"), rope_bounds = "default", rope_full = TRUE, priors=TRUE, iterations = 1000, ...) {
  if (insight::model_info(model)$is_bayesian) {
    data <- insight::get_parameters(model)
  } else {
    data <- model_bootstrap(model, iterations = iterations, ...)
  }

  # Point-estimates
  # TODO: Colour the median in green/red depending on the direction
  parameters <- summarise_posteriors(data, ci = ci, estimate = estimate, test = test, rope_bounds = rope_bounds, rope_full = rope_full)
  return(parameters)
}



