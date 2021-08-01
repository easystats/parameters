#' @title Parameters from Linear Equation System Estimation Models
#' @name model_parameters.systemfit
#'
#' @description
#' Parameters from Linear Equation System Estimation models (from
#' \pkg{systemfit} package).
#'
#' @param model A model from \pkg{systemfit} package.
#' @inheritParams model_parameters.default
#' @export

model_parameters.systemfit <- function(model, ci = 0.95, verbose = TRUE, ...) {
  if (!missing(ci)) {
    if (isTRUE(verbose)) {
      message(insight::format_message("'systemfit' models do not support other levels for confidence intervals than 0.95. Argument 'ci' is ignored."))
    }
    ci <- 0.95
  }

  # estimates
  params <- as.data.frame(summary(model)$coefficients)
  params$Parameter <- c(attributes(params)$row.names)
  colnames(params) <- c("Coefficient", "SE", "t", "p", "Parameter")
  rownames(params) <- NULL

  # confidence intervals
  ci_data <- as.data.frame(unclass(stats::confint(model, level = ci)))
  colnames(ci_data) <- c("CI_low", "CI_high")
  ci_data$Parameter <- c(attributes(ci_data)$row.names)
  rownames(ci_data) <- NULL

  # merge estimates with CIs
  params <- merge(params, ci_data, by = "Parameter", all.x = TRUE)

  # add CI level column
  params$CI <- ci

  # reorder columns
  col_order <- c("Parameter", "Coefficient", "CI", "CI_low", "CI_high", "SE", "t", "p")
  params <- params[col_order[col_order %in% names(params)]]

  # add attributes
  class(params) <- c("parameters_model", "see_parameters_model", class(params))

  params
}
