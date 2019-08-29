#' @inheritParams model_simulate
#' @rdname model_parameters.merMod
#' @export
model_parameters.glmmTMB <- function(model, ci = .95, standardize = "refit", standardize_robust = FALSE, bootstrap = FALSE, p_method = "wald", ci_method = "wald", iterations = 1000, component = c("all", "conditional", "zi", "zero_inflated"), ...) {

  component <- match.arg(component)

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_glmmTMB(model, ci = ci, component = component, ...)
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
.extract_parameters_glmmTMB <- function(model, ci, component, ...) {
  parameters <- insight::get_parameters(model, effects = "fixed", component = component)
  colnames(parameters) <- c("Parameter", "Estimate", "Component")
  original_order <- parameters$.id <- 1:nrow(parameters)

  if (component == "all")
    merge_by <- c("Parameter", "Component")
  else
    merge_by <- "Parameter"

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci, component = component)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter", "Component")]
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

  rownames(parameters) <- NULL
  parameters
}



.get_statistic <- function(model, component = c("all", "conditional", "zi", "zero_inflated"), ...) {
  component <- match.arg(component)

  cs <- .compact_list(stats::coef(summary(model)))
  x <- lapply(names(cs), function(i) {
    data_frame(
      Parameter = insight::find_parameters(model, effects = "fixed", component = i, flatten = TRUE),
      Statistic = as.vector(cs[[i]][, 3]),
      Component = i
    )
  })

  stat <- do.call(rbind, x)
  stat$Component <- .rename_values(stat$Component, "cond", "conditional")
  stat$Component <- .rename_values(stat$Component, "zi", "zero_inflated")

  .filter_component(stat, component)
}
