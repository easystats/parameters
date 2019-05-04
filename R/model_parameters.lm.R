#' Linear Model Parameters
#'
#' Parameters of linear models.
#'
#' @param model Object of class \link{lm}.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters.
#' @param bootstrap Should estimates be based on bootsrapped model? If TRUE, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply.
#' @param ... Arguments passed to or from other methods (e.g., to \link[=standardize.lm]{standardize}).
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' model_parameters(model, standardize = TRUE)
#' @export
model_parameters.lm <- function(model, ci = .95, standardize = FALSE, bootstrap = FALSE, ...) {
  if (bootstrap) {
    return(.model_parameters_bayesian(model, ci = ci, standardize = standardize, ...))
  }
  # Processing
  parameters <- .extract_parameters_lm(model, ci = ci)

  # Standardized
  if (standardize) {
    parameters <- cbind(parameters, standardize_parameters(model)[2])
  }

  return(parameters)
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_lm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("beta", "SE", "t", "p")

  parameters$DoF_residual <- model$df.residual


  parameters <- cbind(
    data_frame("Parameter" = rownames(parameters)),
    parameters,
    ci(model, ci = ci)
  )

  parameters <- parameters[c("Parameter", "beta", "SE", "CI_low", "CI_high", "t", "DoF_residual", "p")]
  rownames(parameters) <- NULL

  parameters
}
