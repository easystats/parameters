#' @export
model_parameters.wbm <- function(model, ci = .95, bootstrap = FALSE, p_method = "wald", ci_method = "wald", iterations = 1000, ...) {

  # Processing
  if (bootstrap) {
    parameters <- parameters_bootstrap(model, iterations = iterations, ci = ci, ...)
  } else {
    parameters <- .extract_parameters_panelr(model, ci = ci, p_method = p_method, ci_method = ci_method, ...)
  }

  attr(parameters, "pretty_names") <- format_parameters(model)
  attr(parameters, "ci") <- ci
  class(parameters) <- c("parameters_model", "see_parameters_model", class(parameters))
  parameters
}




#' @importFrom insight find_parameters
#' @importFrom stats confint
#' @keywords internal
.extract_parameters_panelr <- function(model, ci = .95, p_method = "wald", ci_method = "wald", ...) {
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
