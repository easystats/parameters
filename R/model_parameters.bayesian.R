#' Linear Model Parameters
#'
#' Parameters of linear models.
#'
#' @param model Object of class \link{lm}.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters.
#' @param ... Arguments passed to or from other methods (e.g., to \code{standardize}).
#'
#' @examples
#' \dontrun{
#' model <- rstanarm::stan_glm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' model_parameters(model, standardize = TRUE)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats confint
#' @export
model_parameters.stanreg <- function(model, ci=.90, standardize=FALSE, estimate="median", test = c("pd", "rope"), rope_bounds = "default", n=1000, ...) {

  # ROPE
  if (all(rope_bounds == "default")) {
    rope_bounds <- c(-0.1 * sd(insight::get_response(model)), 0.1 * sd(insight::get_response(model)))
  } else if (!all(is.numeric(rope_bounds)) | length(rope_bounds) != 2) {
    stop("`rope_bounds` should be 'default' or a vector of 2 numeric values (e.g., c(-0.1, 0.1)).")
  }

  # Processing
  parameters <- .extract_parameters_bayesian(model, ci, estimate=estimate, test = test, rope_bounds = rope_bounds, n=n)

  # Standardized
  if (standardize) {
    std_model <- standardize(model, robust = FALSE)
    std_parameters <- .extract_parameters_bayesian(std_model, ci=ci, estimate=estimate, test = NULL, rope_bounds = rope_bounds, n=n)
    names(std_parameters) <- paste0("Std_", names(std_parameters))

    parameters <- cbind(parameters, std_parameters[names(std_parameters) != "Std_Parameter"])
  }

  return(parameters)
}








#' @keywords internal
.extract_parameters_bayesian <- function(model, ci=.90, estimate="median", test = c("pd", "rope"), rope_bounds = c(-0.1, 0.1), n=1000, ...){

  if(insight::model_info(model)$is_bayesian){
    data <- insight::get_parameters(model)
  } else{
    data <- model_bootstrap(model, n=n)
  }

  # Point-estimates
  # TODO: Colour the median in green/red depending on the direction
  if(estimate == "median"){
    parameters <- data.frame(
      "Parameter" = colnames(data),
      "Median" = sapply(data, median),
      "MAD" = sapply(data, mad)
    )
  } else if(estimate == "mean"){
    parameters <- data.frame(
      "Parameter" = colnames(data),
      "Mean" = sapply(data, mean),
      "SD" = sapply(data, sd)
    )
  } else if(estimate == "MAP"){
    parameters <- data.frame(
      "Parameter" = colnames(data),
      "MAP" = sapply(data, bayestestR::map_estimate)
    )
  } else{
    stop("`estimate` must be 'median', 'mean' or 'MAP'.")
  }

  # CI
  if(!is.null(ci)){
    if(length(ci) > 1){
      hdi <- sapply(data, bayestestR::hdi, ci=ci, simplify = FALSE)
      for(i in names(hdi)){
        current_hdi <- hdi[[i]]

        hdi_low <- as.data.frame(t(setNames(current_hdi$CI_low, as.numeric(current_hdi$CI))))
        names(hdi_low) <- paste0("CI_low_", names(hdi_low))

        hdi_high <- as.data.frame(t(setNames(current_hdi$CI_high, as.numeric(current_hdi$CI))))
        names(hdi_high) <- paste0("CI_high_", names(hdi_high))

        hdi[[i]] <- cbind(hdi_low, hdi_high)
      }
      hdi <- bayestestR::flatten_list(hdi)
      hdi <- hdi[names(hdi) != "name"]
    } else{
      hdi <- as.data.frame(t(sapply(data, bayestestR::hdi, ci=ci)))
      hdi <- hdi[c("CI_low", "CI_high")]
    }
    parameters <- cbind(parameters, hdi)
  }


  # Effect Existence
  test_list <- tolower(c(test))
  if("pd" %in% test_list | "p_direction" %in% test_list | "pdir" %in% test_list | "mpe" %in% test_list){
    parameters$pd <- sapply(data, bayestestR::p_direction)
  }
  if("rope" %in% test_list | "equivalence" %in% test_list | "equi" %in% test_list){
    if(length(ci) > 1){
      results_rope <- sapply(data, bayestestR::equivalence_test, bounds=rope_bounds, ci=ci, simplify=FALSE)
      for(i in names(results_rope)){
        current_rope <- results_rope[[i]]

        rope_percentage <- as.data.frame(t(setNames(current_rope$ROPE_Percentage, as.numeric(current_rope$CI))))
        names(rope_percentage) <- paste0("CI_", names(rope_percentage), "_ROPE_Percentage")

        rope_equivalence <- as.data.frame(t(setNames(current_rope$ROPE_Equivalence, as.numeric(current_rope$CI))))
        names(rope_equivalence) <- paste0("CI_", names(rope_equivalence), "_ROPE_Equivalence")

        results_rope[[i]] <- cbind(rope_percentage, rope_equivalence)
      }
      results_rope <- bayestestR::flatten_list(results_rope)
      results_rope <- results_rope[names(results_rope) != "name"]
    } else{
      results_rope <- as.data.frame(t(sapply(data, bayestestR::equivalence_test, bounds=rope_bounds, ci=ci)))
      results_rope <- results_rope[c("ROPE_Percentage", "ROPE_Equivalence")]
    }
    parameters <- cbind(parameters, results_rope)
  }
  if("p_map" %in% test_list | "pmap" %in% test_list){
    parameters$p_MAP <- sapply(data, bayestestR::p_map)
  }
  # TODO: add p_ROPE, but first must enhance its implementation in bayestestR


  rownames(parameters) <- NULL
  return(parameters)
}