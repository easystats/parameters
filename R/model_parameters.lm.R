#' Parameters of (General) Linear Models
#'
#' Extract and compute indices and measures to describe parameters of (general) linear models (GLMs).
#'
#' @param model Model object.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param bootstrap Should estimates be based on bootstrapped model? If \code{TRUE}, then arguments of \link[=model_parameters.stanreg]{Bayesian regressions} apply (see also \code{\link[=parameters_bootstrap]{parameters_bootstrap()}}).
#' @param iterations The number of bootstrap replicates. This only apply in the case of bootstrapped frequentist models.
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' model_parameters(model)
#' model_parameters(model, bootstrap = TRUE)
#'
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model)
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.lm <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, ...) {

  # Type of model
  info <- insight::model_info(model)

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_glm(model, ci = ci, linear = info$is_linear)
  }


  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}




#' @export
model_parameters.glm <- model_parameters.lm
