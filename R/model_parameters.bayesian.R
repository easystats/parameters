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
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param standardize Add standardized parameters. Default to FALSE as this re-fits the model and can thus take some time.
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/2_IndicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test What \href{https://easystats.github.io/bayestestR/articles/3_IndicesExistenceComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope" or "p_map".
#' @param rope_bounds \href{https://easystats.github.io/bayestestR/articles/1_IndicesDescription.html#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
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
  parameters <- data.frame("Parameter" = colnames(data))
  if ("median" %in% c(estimate)) {
    parameters$Median <- sapply(data, median)
    parameters$MAD <- sapply(data, mad)
  }
  if ("mean" %in% c(estimate)) {
    parameters$Mean <- sapply(data, mean)
    parameters$SD <- sapply(data, sd)
  }
  if ("map" %in% c(estimate)) {
    parameters$MAP <- sapply(data, bayestestR::map_estimate)
  }

  # CI
  if (!is.null(ci)) {
    if (length(ci) > 1) {
      hdi <- sapply(data, bayestestR::hdi, ci = ci, simplify = FALSE)
      for (i in names(hdi)) {
        current_hdi <- hdi[[i]]

        hdi_low <- as.data.frame(t(setNames(current_hdi$CI_low, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_low) <- paste0("CI_low_", names(hdi_low))

        hdi_high <- as.data.frame(t(setNames(current_hdi$CI_high, as.numeric(current_hdi$CI))), stringsAsFactors = FALSE)
        names(hdi_high) <- paste0("CI_high_", names(hdi_high))

        hdi[[i]] <- cbind(hdi_low, hdi_high)
      }
      hdi <- flatten_list(hdi)
      hdi <- hdi[names(hdi) != "name"]
    } else {
      hdi <- as.data.frame(t(sapply(data, bayestestR::hdi, ci = ci)), stringsAsFactors = FALSE)
      hdi <- hdi[c("CI_low", "CI_high")]
    }
    hdi <- sapply(hdi, as.numeric)
    parameters <- cbind(parameters, hdi)
  }


  # Effect Existence
  if (!is.null(test)) {
    test_list <- tolower(c(test))
    if ("pd" %in% test_list | "p_direction" %in% test_list | "pdir" %in% test_list | "mpe" %in% test_list) {
      parameters$pd <- sapply(data, bayestestR::p_direction)
    }
    if ("rope" %in% test_list | "equivalence" %in% test_list | "equi" %in% test_list) {
      if (length(ci) == 1 | rope_full) {
        if(rope_full){
          results_rope <- as.data.frame(t(sapply(data, bayestestR::equivalence_test, bounds = rope_bounds, ci = 1)), stringsAsFactors = FALSE)
        } else{
          results_rope <- as.data.frame(t(sapply(data, bayestestR::equivalence_test, bounds = rope_bounds, ci = ci)), stringsAsFactors = FALSE)
        }
        results_rope <- results_rope[c("ROPE_Percentage", "ROPE_Equivalence")]
        results_rope$ROPE_Percentage <- as.numeric(results_rope$ROPE_Percentage)
        results_rope$ROPE_Equivalence <- as.character(results_rope$ROPE_Equivalence)


      } else {
        results_rope <- sapply(data, bayestestR::equivalence_test, bounds = rope_bounds, ci = ci, simplify = FALSE)
        for (i in names(results_rope)) {
          current_rope <- results_rope[[i]]

          rope_percentage <- as.data.frame(t(setNames(current_rope$ROPE_Percentage, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_percentage) <- paste0("CI_", names(rope_percentage), "_ROPE_Percentage")
          rope_percentage <- sapply(rope_percentage, as.numeric)

          rope_equivalence <- as.data.frame(t(setNames(current_rope$ROPE_Equivalence, as.numeric(current_rope$CI))), stringsAsFactors = FALSE)
          names(rope_equivalence) <- paste0("CI_", names(rope_equivalence), "_ROPE_Equivalence")
          rope_equivalence <- sapply(rope_equivalence, as.character)

          results_rope[[i]] <- cbind(rope_percentage, rope_equivalence)
        }
        results_rope <- flatten_list(results_rope)
        results_rope <- results_rope[names(results_rope) != "name"]
      }
      parameters <- cbind(parameters, results_rope)
    }
    if ("p_map" %in% test_list | "pmap" %in% test_list) {
      parameters$p_MAP <- sapply(data, bayestestR::p_map)
    }
    # TODO: add p_ROPE, but first must enhance its implementation in bayestestR
  }


  rownames(parameters) <- NULL
  return(parameters)
}



