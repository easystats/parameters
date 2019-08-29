#' @inheritParams model_simulate
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, bootstrap = FALSE, p_method = "wald", ci_method = "wald", iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  # fix argument, if model has no zi-part
  if (!insight::model_info(model)$is_zero_inflated && component != "conditional") {
    component <- "conditional"
  }

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_glmmTMB(model, ci = ci, component = component, ...)
  }

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}

#' @export
model_parameters.MixMod <- model_parameters.glmmTMB


#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glmmTMB <- function(model, ci, component, ...) {
  parameters <- insight::get_parameters(model, effects = "fixed", component = component)
  colnames(parameters) <- c("Parameter", "Estimate", "Component")
  original_order <- parameters$.id <- 1:nrow(parameters)

  merge_by <- c("Parameter", "Component")

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci, component = component)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", merge_by)]
    parameters <- merge(parameters, ci_df, by = merge_by)
  } else {
    ci_cols <- c()
  }


  # p value
  parameters <- merge(parameters, p_value(model, component = component), by = merge_by)

  # standard error
  parameters <- merge(parameters, standard_error(model, component = component), by = merge_by)

  # test statistic
  parameters <- merge(parameters, .get_statistic(model, component = component), by = merge_by)

  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub("Statistic", "z", names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))

  # Reorder
  col_order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df", "df_residual", "p", "Component")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]

  # remove Component column if not needed
  if (length(unique(parameters$Component)) == 1) parameters$Component <- NULL

  rownames(parameters) <- NULL
  parameters
}
