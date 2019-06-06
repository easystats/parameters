#' Linear Model Parameters
#'
#' Parameters of linear models.
#'
#' @param model Object of class \link{lm}.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters. Can be FALSE or a character indicating the standardization method (see \link{parameters_standardize}).
#' @param standardize_robust Robust standardization. See \link{parameters_standardize}.
#' @param bootstrap Should estimates be based on bootsrapped model? If TRUE, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply.
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootsrapped frequentist models.
#' @param ... Arguments passed to or from other methods (e.g., to \link[=standardize.lm]{standardize}).
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model, standardize = "refit")
#' model_parameters(model, bootstrap = TRUE)
#' @export
model_parameters.lm <- function(model, ci = .95, standardize = "refit", standardize_robust = FALSE, bootstrap = FALSE, iterations = 1000, ...) {

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_lm(model, ci = ci)
  }


  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
    standardize <- "refit"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    parameters <- cbind(parameters, parameters_standardize(model, method = standardize, robust = standardize_robust)[2])
  }

  parameters
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_lm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("Coefficient", "SE", "t", "p")

  parameters$DoF_residual <- model$df.residual
  parameters$Parameter <- row.names(parameters)

  # CI
  col_order <- parameters$Parameter
  parameters <- merge(parameters, ci(model, ci = ci), by = "Parameter")
  parameters <- parameters[match(col_order, parameters$Parameter), ]

  parameters <- parameters[c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t", "DoF_residual", "p")]
  rownames(parameters) <- NULL

  parameters
}
