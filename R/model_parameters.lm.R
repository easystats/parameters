#' Linear Model Parameters
#'
#' Parameters of linear models.
#'
#' @param model Object of class \link{lm}.
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#' @param standardize Add standardized parameters.
#' @param ... Arguments passed to or from other methods (e.g., to \code{standardize}).
#'
#' @examples
#' model <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' model_parameters(model, standardize = TRUE)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats confint
#' @export
model_parameters.lm <- function(model, ci = .95, standardize = FALSE, ...) {

  # Processing
  parameters <- .extract_parameters_lm(model, ci)

  # Standardized
  if (standardize) {
    std_model <- standardize(model, robust = FALSE)
    std_parameters <- .extract_parameters_lm(std_model, ci)
    names(std_parameters) <- paste0("Std_", names(std_parameters))

    parameters <- cbind(parameters, std_parameters[c("Std_beta", "Std_SE", "Std_CI_low", "Std_CI_high")])
  }

  return(parameters)
}






#' @keywords internal
.extract_parameters_lm <- function(model, ci=.95){

  parameters <- as.data.frame(summary(model)$coefficients)
  names(parameters) <- c("beta", "SE", "t", "p")

  parameters$DoF_residual <- model$df.residual
  parameters <- parameters[c("beta", "SE", "t", "DoF_residual", "p")]

  ci_table <- as.data.frame(confint(model, level=ci))
  names(ci_table) <- c("CI_low", "CI_high")


  parameters <- cbind(
    data.frame("parameters" = rownames(parameters)),
    parameters,
    ci_table
  )

  rownames(parameters) <- NULL

  parameters
}