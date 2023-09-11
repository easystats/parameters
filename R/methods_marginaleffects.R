# x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# model <- marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")

# model_parameters ----------------

#' @rdname model_parameters.averaging
#' @export
model_parameters.marginaleffects <- function(model,
                                             ci = 0.95,
                                             exponentiate = FALSE,
                                             ...) {
  insight::check_if_installed("marginaleffects")
  out <- insight::standardize_names(
    marginaleffects::tidy(model, conf_level = ci, ...),
    style = "easystats"
  )

  # contrast_ columns provide indispensable information about the comparisons
  colnames(out)[colnames(out) == "contrast"] <- "Comparison"
  colnames(out) <- gsub("^contrast_", "Comparison: ", colnames(out))

  out <- .safe(.add_model_parameters_attributes(out, model, ci, exponentiate = exponentiate, ...), out)

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))

  if (inherits(model, "marginalmeans")) {
    attr(out, "coefficient_name") <- "Marginal Means"
  } else if (inherits(model, "comparisons")) {
    attr(out, "coefficient_name") <- "Estimate"
    attr(out, "title") <- "Contrasts between Adjusted Predictions"
    if ("Type" %in% colnames(out)) {
      attr(out, "prediction_type") <- out$Type[1]
    }
  } else if (inherits(model, "slopes")) {
    attr(out, "coefficient_name") <- "Slope"
  } else if (inherits(model, "predictions")) {
    attr(out, "coefficient_name") <- "Predicted"
  } else if (inherits(model, "hypotheses")) {
    attr(out, "coefficient_name") <- "Estimate"
  }

  # exponentiate coefficients and SE/CI, if requested
  out <- .exponentiate_parameters(out, model = NULL, exponentiate)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}

#' @rdname model_parameters.averaging
#' @export
model_parameters.comparisons <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.marginalmeans <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.hypotheses <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.slopes <- model_parameters.marginaleffects


#' @rdname model_parameters.averaging
#' @export
model_parameters.predictions <- function(model,
                                         ci = 0.95,
                                         exponentiate = TRUE,
                                         ...) {
  insight::check_if_installed("marginaleffects")

  out <- datawizard::data_rename(model, "estimate", "predicted")
  out <- datawizard::data_relocate(out, "predicted", before = 1)
  out <- insight::standardize_names(out, style = "easystats")
  out <- insight::standardize_column_order(out, style = "easystats")

  # remove and reorder some columns
  out$rowid <- out$Type <- NULL
  out <- datawizard::data_relocate(out, select = attributes(model)$newdata_at, after = "Predicted")

  # extract response, remove from data frame
  reg_model <- attributes(model)$model
  if (!is.null(reg_model) && insight::is_model(reg_model)) {
    resp <- insight::find_response(reg_model)
    out[[resp]] <- NULL
  }

  out <- .safe(.add_model_parameters_attributes(out, model, ci, exponentiate = exponentiate, ...), out)

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "coefficient_name") <- "Predicted"
  attr(out, "no_caption") <- TRUE

  # exponentiate coefficients and SE/CI, if requested
  out <- .exponentiate_parameters(out, model = NULL, exponentiate)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}
