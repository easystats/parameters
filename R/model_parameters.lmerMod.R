#' Linear Mixed Model Parameters
#'
#' Parameters of linear mixed models.
#'
#' @inheritParams model_parameters.lm
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(mpg ~ wt + (1|gear), data = mtcars)
#' model_parameters(model, standardize = TRUE)
#' }
#'
#' @export
model_parameters.lmerMod <- function(model, ci = .95, standardize = FALSE, bootstrap = FALSE, ...) {
  if (bootstrap) {
    return(.model_parameters_bayesian(model, ci = ci, standardize = standardize, ...))
  }
  # Processing
  parameters <- .extract_parameters_mixed_lm(model, ci = ci)

  # Standardized
  if (standardize) {
    std_model <- standardize(model, ...)
    std_parameters <- .extract_parameters_mixed_lm(std_model, ci)
    names(std_parameters) <- paste0("Std_", names(std_parameters))

    # parameters <- cbind(parameters, std_parameters[c("Std_beta", "Std_SE", "Std_CI_low", "Std_CI_high")])
    parameters <- cbind(parameters, std_parameters[c("Std_beta", "Std_SE")])
  }

  return(parameters)
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed_lm <- function(model, ci = .95) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  names(parameters) <- c("beta", "SE", "t")

  # parameters$DoF_residual <- residuals(model)
  # parameters <- parameters[c("beta", "SE", "t", "DoF_residual", "p")]

  # ci_table <- as.data.frame(confint(model, level = ci), stringsAsFactors = FALSE)
  # names(ci_table) <- c("CI_low", "CI_high")


  parameters <- cbind(
    data_frame("Parameter" = rownames(parameters)),
    parameters
    # ci_table
  )

  rownames(parameters) <- NULL

  parameters
}
