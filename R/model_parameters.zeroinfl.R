#' Parameters from Zero-Inflated Models
#'
#' Parameters from zero-inflated models.
#'
#' @param model A model with zero-inflation component.
#' @inheritParams model_parameters.default
#' @inheritParams simulate_model
#'
#' @seealso \code{\link[=standardize_names]{standardize_names()}} to rename
#'   columns into a consistent, standardized naming scheme.
#'
#' @examples
#' library(parameters)
#' if (require("pscl")) {
#'   data("bioChemists")
#'   model <- zeroinfl(art ~ fem + mar + kid5 + ment | kid5 + phd, data = bioChemists)
#'   model_parameters(model)
#' }
#' @return A data frame of indices related to the model's parameters.
#' @inheritParams simulate_model
#' @export
model_parameters.zeroinfl <- function(model, ci = .95, bootstrap = FALSE, iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), standardize = NULL, exponentiate = FALSE, robust = FALSE, ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }


  # Processing
  if (bootstrap) {
    parameters <- bootstrap_parameters(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_generic(model, ci = ci, component = component, standardize = standardize, robust = robust, ...)
  }


  if (exponentiate) parameters <- .exponentiate_parameters(parameters)
  parameters <- .add_model_parameters_attributes(parameters, model, ci, exponentiate, ...)
  attr(parameters, "object_name") <- deparse(substitute(model), width.cutoff = 500)
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))

  parameters
}


#' @export
model_parameters.hurdle <- model_parameters.zeroinfl

#' @export
model_parameters.zerocount <- model_parameters.zeroinfl
