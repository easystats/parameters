#' Linear Model Parameters
#'
#' Parameters of general linear models.
#'
#' @inheritParams model_parameters.lm
#'
#' @examples
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model, standardize = "refit")
#' @export
model_parameters.glm <- function(model, ci = .95, standardize = "refit", standardize_robust = FALSE, bootstrap = FALSE, iterations = 1000, ...) {

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else{
    parameters <- .extract_parameters_glm(model, ci = ci)
  }


  # Standardized
  if (standardize != FALSE & !is.null(standardize)) {
    if (standardize == TRUE) {
      warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
      standardize <- "refit"
    }
    parameters <- cbind(parameters, parameters_standardize(model, method = standardize, robust = standardize_robust)[2])
  }

  return(parameters)
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("Coefficient", "SE", "z", "p")

  parameters$DoF_residual <- model$df.residual
  parameters$Parameter <- row.names(parameters)

  # CI
  col_order <- parameters$Parameter
  parameters <- merge(parameters, ci(model, ci = ci), by="Parameter")
  parameters <- parameters[match(col_order, parameters$Parameter), ]

  parameters <- parameters[c("Parameter", "Coefficient", "SE", "CI_low", "CI_high", "z", "DoF_residual", "p")]
  rownames(parameters) <- NULL

  parameters
}
