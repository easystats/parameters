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
#' model_parameters(model, standardize = "refit")
#'
#' model <- glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#' model_parameters(model)
#'
#' \donttest{
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' model_parameters(model, standardize = "smart", bootstrap = TRUE, iterations = 50)
#' }
#'
#' @return A data frame of indices related to the model's parameters.
#' @export
model_parameters.merMod <- function(model, ci = .95, standardize = "refit", standardize_robust = FALSE, bootstrap = FALSE, p_method = "wald", ci_method = "wald", iterations = 1000, ...) {

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_mixed(model, ci = ci, p_method = p_method, ci_method = ci_method, ...)
  }


  # Standardized
  if (isTRUE(standardize)) {
    warning("Please set the `standardize` method explicitly. Set to \"refit\" by default.")
    standardize <- "refit"
  }

  if (!is.null(standardize) && !is.logical(standardize)) {
    parameters <- cbind(parameters, parameters_standardize(model, method = standardize, robust = standardize_robust)[2])
  }

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}





#' @importFrom stats confint
#' @keywords internal
.extract_parameters_mixed <- function(model, ci = .95, p_method = "wald", ci_method = "wald", ...) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)
  parameters$Parameter <- row.names(parameters)
  original_order <- parameters$Parameter

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci, method = ci_method)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]

    col_order <- parameters$Parameter
    parameters <- merge(parameters, ci_df, by = "Parameter")
    parameters <- parameters[match(col_order, parameters$Parameter), ]
  } else {
    ci_cols <- c()
  }


  # p value
  if ("Pr(>|z|)" %in% names(parameters)) {
    names(parameters)[grepl("Pr(>|z|)", names(parameters), fixed = TRUE)] <- "p"
  } else {
    if (insight::model_info(model)$is_linear) {
      if (p_method == "kenward") {
        parameters$df <- dof_kenward(model)
        parameters <- merge(parameters, p_value(model, method = "kenward", dof = parameters$DoF), by = "Parameter")
      } else {
        parameters <- merge(parameters, p_value(model, method = p_method), by = "Parameter")
      }
    } else {
      parameters <- merge(parameters, p_value(model), by = "Parameter")
    }
  }

  # Rematch order after merging
  parameters <- parameters[match(parameters$Parameter, original_order), ]
  row.names(parameters) <- NULL

  # Renaming
  names(parameters) <- gsub("Std. Error", "SE", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))
  names(parameters) <- gsub("t value", "t", names(parameters))
  names(parameters) <- gsub("z value", "z", names(parameters))

  # Reorder
  order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df", "df_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  rownames(parameters) <- NULL
  parameters
}
