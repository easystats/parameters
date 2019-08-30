#' @importFrom stats confint
#' @keywords internal
.extract_parameters_generic <- function(model, ci, component, merge_by = c("Parameter", "Component"), ...) {
  parameters <- insight::get_parameters(model, effects = "fixed", component = component)
  .statistic <- .get_statistic(model, component = component)

  if (inherits(model, "polr")) {
    parameters$parameter <- gsub("Intercept: ", "", parameters$parameter, fixed = TRUE)
  }

  if ("component" %in% names(parameters)) {
    cn <- c("Parameter", "Estimate", "Component")
  } else {
    cn <- c("Parameter", "Estimate")
  }

  colnames(parameters) <- cn
  original_order <- parameters$.id <- 1:nrow(parameters)

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
  parameters <- merge(parameters, .statistic, by = merge_by)

  # Rematch order after merging
  parameters <- parameters[match(original_order, parameters$.id), ]

  # Renaming
  names(parameters) <- gsub("Statistic", attr(.statistic, "statistic", exact = TRUE), names(parameters))
  names(parameters) <- gsub("Estimate", "Coefficient", names(parameters))

  # Reorder
  col_order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df", "df_residual", "p", "Component")
  parameters <- parameters[col_order[col_order %in% names(parameters)]]

  # remove Component column if not needed
  if (length(unique(parameters$Component)) == 1) parameters$Component <- NULL

  rownames(parameters) <- NULL
  parameters
}



#' @importFrom stats confint
#' @keywords internal
.extract_parameters_glm <- function(model, ci = .95, linear = FALSE) {
  parameters <- as.data.frame(summary(model)$coefficients, stringsAsFactors = FALSE)

  if (linear) {
    names(parameters) <- c("Coefficient", "SE", "t", "p")
  } else {
    names(parameters) <- c("Coefficient", "SE", "z", "p")
  }


  parameters$df_residual <- model$df.residual
  parameters$Parameter <- row.names(parameters)

  # CI
  if (!is.null(ci)) {
    ci_df <- ci(model, ci = ci)
    if (length(ci) > 1) ci_df <- bayestestR::reshape_ci(ci_df)
    ci_cols <- names(ci_df)[!names(ci_df) %in% c("CI", "Parameter")]

    col_order <- parameters$Parameter
    parameters <- merge(parameters, ci_df, by = "Parameter")
    parameters <- parameters[match(col_order, parameters$Parameter), ]
  } else {
    ci_cols <- c()
  }


  # Reorder
  order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  rownames(parameters) <- NULL
  parameters
}



#' @importFrom insight find_parameters
#' @importFrom stats confint
#' @keywords internal
.extract_parameters_panelr <- function(model, ci = .95, ci_method = "wald", ...) {
  parameters <- as.data.frame(model@summ$coeftable, stringsAsFactors = FALSE)
  parameters$Parameter <- insight::find_parameters(model, effects = "fixed", flatten = TRUE)
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
  if (!("p" %in% names(parameters))) {
    parameters <- merge(parameters, p_value(model), by = "Parameter")
  }

  # Rematch order after merging
  parameters <- parameters[match(parameters$Parameter, original_order), ]
  row.names(parameters) <- NULL

  # Renaming
  names(parameters) <- gsub("S.E.", "SE", names(parameters))
  names(parameters) <- gsub("Est.", "Coefficient", names(parameters))
  names(parameters) <- gsub("t val.", "t", names(parameters))
  names(parameters) <- gsub("z val.", "z", names(parameters))
  names(parameters) <- gsub("d.f.", "df", names(parameters))

  # Reorder
  order <- c("Parameter", "Coefficient", "SE", ci_cols, "t", "z", "df", "df_residual", "p")
  parameters <- parameters[order[order %in% names(parameters)]]

  rownames(parameters) <- NULL
  parameters
}

