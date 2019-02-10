#' Linear Model Parameters
#'
#' Parameters of general linear models.
#'
#' @inheritParams model_parameters.lm
#'
#' @examples
#' model <- glm(vs ~ wt + cyl, data = mtcars, family="binomial")
#' model_parameters(model, standardize = TRUE)
#' @export
model_parameters.glm <- function(model, ci = .95, standardize = FALSE, bootstrap = FALSE, ...) {
  if (bootstrap) {
    return(.model_parameters_bayesian(model, ci = ci, standardize = standardize, ...))
  }
  # Processing
  parameters <- .extract_parameters_glm(model, ci = ci)

  # Standardized
  if (standardize) {
    std_model <- standardize(model, ...)
    std_parameters <- .extract_parameters_glm(std_model, ci)
    names(std_parameters) <- paste0("Std_", names(std_parameters))

    parameters <- cbind(parameters, std_parameters[c("Std_beta", "Std_SE", "Std_CI_low", "Std_CI_high")])
  }

  return(parameters)
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("beta", "SE", "z", "p")

  parameters$DoF_residual <- model$df.residual
  parameters <- parameters[c("beta", "SE", "z", "DoF_residual", "p")]

  ci_table <- suppressMessages(as.data.frame(confint(model, level = ci), stringsAsFactors = FALSE))
  names(ci_table) <- c("CI_low", "CI_high")


  parameters <- cbind(
    data_frame("Parameter" = rownames(parameters)),
    parameters,
    ci_table
  )

  rownames(parameters) <- NULL

  parameters
}
