#' Linear Model Parameters
#'
#' Parameters of general linear models.
#'
#' @inheritParams model_parameters.lm
#'
#' @examples
#' model <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' model_parameters(model, standardize = TRUE)
#' @export
model_parameters.glm <- function(model, ci = .95, standardize = FALSE, bootstrap = FALSE, ...) {
  if (bootstrap) {
    return(.model_parameters_bayesian(model, ci = ci, standardize = standardize, ...))
  }
  # Processing
  parameters <- .extract_parameters_glm(model, ci = ci)

  # Standardized
  if (standardize != FALSE & !is.null(standardize)) {
    if(standardize == TRUE){
      warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
      standardize <- "refit"
    }
    parameters <- cbind(parameters, standardize_parameters(model, method = standardize)[2])
  }

  return(parameters)
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("beta", "SE", "z", "p")

  parameters$DoF_residual <- model$df.residual

  parameters <- cbind(
    data_frame("Parameter" = rownames(parameters)),
    parameters,
    ci(model, ci = ci)
  )

  parameters <- parameters[c("Parameter", "beta", "SE", "CI_low", "CI_high", "z", "DoF_residual", "p")]
  rownames(parameters) <- NULL

  parameters
}
