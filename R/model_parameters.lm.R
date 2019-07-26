#' Parameters of (General) Linear Models
#'
#' Extract and compute indices and measures to describe parameters of (general) linear models (GLMs).
#'
#' @param model Frequentist regression object.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters. Can be FALSE or a character indicating the standardization method (see \code{\link{parameters_standardize}}).
#' @param standardize_robust Robust standardization. See \code{\link{parameters_standardize}}.
#' @param bootstrap Should estimates be based on bootsrapped model? If TRUE, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply.
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootsrapped frequentist models.
#' @param ... Arguments passed to or from other methods (e.g., to \link[=standardize.lm]{standardize}).
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model, standardize = "refit")
#' model_parameters(model, bootstrap = TRUE)
#'
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#' @export
model_parameters.lm <- function(model, ci = .95, standardize = "refit", standardize_robust = FALSE, bootstrap = FALSE, iterations = 1000, ...) {

  # Type of model
  info <- insight::model_info(model)

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_glm(model, ci = ci, linear = info$is_linear)
  }


  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
    standardize <- "refit"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    parameters <- cbind(parameters, parameters_standardize(model, method = standardize, robust = standardize_robust)[2])
  }

  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  attr(parameters, "clean_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  parameters
}




#' @export
model_parameters.glm <- model_parameters.lm






#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95, linear = FALSE) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)

  if (linear) {
    names(parameters) <- c("Coefficient", "SE", "t", "p")
  } else{
    names(parameters) <- c("Coefficient", "SE", "z", "p")
  }


  parameters$df_residual <- model$df.residual
  parameters$Parameter <- row.names(parameters)

  # CI
  col_order <- parameters$Parameter
  parameters <- merge(parameters, ci(model, ci = ci), by = "Parameter")
  parameters <- parameters[match(col_order, parameters$Parameter), ]

  if (linear) {
    parameters <- parameters[c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "t", "df_residual", "p")]
  } else{
    parameters <- parameters[c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "z", "df_residual", "p")]
  }
  rownames(parameters) <- NULL
  parameters
}