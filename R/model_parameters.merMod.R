#' Mixed Model Parameters
#'
#' Parameters of mixed models.
#'
#' @param model A mixed model.
#' @inheritParams model_parameters.lm
#' @param p_method Method for computing p values. See \code{\link[=p_value]{p_value()}}.
#' @param ci_method Method for computing confidence intervals (CI). See \code{\link[=ci]{ci()}}.
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' library(lme4)
#' library(glmmTMB)
#'
#' model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model)
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' model_parameters(model)
#' \donttest{
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model, bootstrap = TRUE, iterations = 50)
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model, ci = .95, bootstrap = FALSE, p_method = "wald", ci_method = "wald", iterations = 1000, ...) {

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_mixed(model, ci = ci, p_method = p_method, ci_method = ci_method, ...)
  }


  parameters <- .add_model_parameters_attributes(parameters, model, ci, ...)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}
