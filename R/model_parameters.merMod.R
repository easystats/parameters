#' Linear Mixed Model Parameters
#'
#' Parameters of linear mixed models.
#'
#' @inheritParams model_parameters.lm
#' @param p_method Method for computing p values. See \link{p_value}.
#' @param ci_method Method for computing confidence intervals (CI). See \link{ci}.
#'
#' @examples
#' \dontrun{
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model, standardize = TRUE)
#' }
#'
#' @export
model_parameters.merMod <- function(model, ci = .95, standardize = FALSE, bootstrap = FALSE, p_method = "wald", ci_method = "wald", ...) {
  if (bootstrap) {
    return(.model_parameters_bayesian(model, ci = ci, standardize = standardize, ...))
  }
  # Processing
  parameters <- .extract_parameters_mixed(model, ci = ci, p_method = p_method, ci_method = ci_method, ...)

  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
    standardize <- "refit"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    parameters <- cbind(parameters, standardize_parameters(model, method = standardize)[2])
  }

  parameters
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model, ci = .95, p_method = "wald", ci_method = "wald", ...) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)

  # p value
  if ("Pr(>|z|)" %in% names(parameters)) {
    names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
  } else {
    if (insight::model_info(model)$is_linear) {
      if (p_method == "kenward") {
        parameters$DoF <- dof_kenward(model)
        parameters$p <- p_value(model, method = "kenward", dof = parameters$DoF)
      } else {
        parameters$p <- p_value(model, method = p_method)
      }
    } else {
      parameters$p <- p_value(model)
    }
  }

  # CI
  parameters <- cbind(parameters, ci(model, method = ci_method))

  # Renaming
  names(parameters) <- gsub("Std. Error", "SE", names(parameters))
  names(parameters) <- gsub("Estimate", "beta", names(parameters))
  names(parameters) <- gsub("t value", "t", names(parameters))
  names(parameters) <- gsub("z value", "z", names(parameters))

  parameters$Parameter <- row.names(parameters)
  rownames(parameters) <- NULL

  # Reorder
  order <- c("Parameter", "beta", "SE", "CI_low", "CI_high", "t", "z", "DoF", "DoF_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  parameters
}
