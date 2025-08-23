# x <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
# model <- marginaleffects(x, newdata = insight::get_datagrid(x, at = "Species"), variables = "Petal.Length")

# model_parameters ----------------

#' @export
model_parameters.marginaleffects <- function(model,
                                             ci = 0.95,
                                             exponentiate = FALSE,
                                             verbose = TRUE,
                                             ...) {
  insight::check_if_installed("marginaleffects")

  # Bayesian models have posterior draws as attribute
  is_bayesian <- !is.null(suppressWarnings(marginaleffects::get_draws(model, "PxD")))

  if (is_bayesian) {
    # Bayesian
    tidy_model <- suppressWarnings(bayestestR::describe_posterior(
      model,
      ci = ci,
      verbose = verbose,
      ...
    ))
  } else {
    # handle non-Bayesian models
    tidy_model <- marginaleffects::tidy(model, conf_level = ci, ...)
  }

  out <- .rename_reserved_marginaleffects(tidy_model)

  # need to standardize names for non-Bayesian models. Bayesian models have
  # been processed through describe_posterior() already
  if (!is_bayesian) {
    out <- insight::standardize_names(out, style = "easystats")
  }

  # in case data grid contained column names that are reserved words,
  # rename those back now...
  colnames(out) <- gsub("#####$", "", colnames(out))

  # contrast_ columns provide indispensable information about the comparisons
  colnames(out)[colnames(out) == "contrast"] <- "Comparison"
  colnames(out) <- gsub("^contrast_", "Comparison: ", colnames(out))

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))

  # do not print or report these columns
  out <- out[, !colnames(out) %in% c("predicted_lo", "predicted_hi"), drop = FALSE]

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

  # add further information as attributes
  out <- .safe(
    .add_model_parameters_attributes(
      out,
      model = model,
      ci = ci,
      exponentiate = exponentiate,
      verbose = verbose,
      ...
    ),
    out
  )

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}

#' @export
model_parameters.comparisons <- model_parameters.marginaleffects


#' @export
model_parameters.marginalmeans <- model_parameters.marginaleffects


#' @export
model_parameters.hypotheses <- model_parameters.marginaleffects


#' @export
model_parameters.slopes <- model_parameters.marginaleffects


#' @export
model_parameters.predictions <- function(model,
                                         ci = 0.95,
                                         exponentiate = FALSE,
                                         verbose = TRUE,
                                         ...) {
  insight::check_if_installed("marginaleffects")

  # Bayesian models have posterior draws as attribute
  is_bayes <- !is.null(suppressWarnings(marginaleffects::get_draws(model, "PxD")))

  if (is_bayes) {
    # Bayesian
    out <- suppressWarnings(bayestestR::describe_posterior(
      model,
      ci = ci,
      verbose = verbose,
      ...
    ))
  } else {
    # handle non-Bayesian models
    out <- .rename_reserved_marginaleffects(model)
    out <- datawizard::data_rename(out, "estimate", "predicted")
    out <- datawizard::data_relocate(out, "predicted", before = 1)
    out <- insight::standardize_names(out, style = "easystats")
  }

  out <- insight::standardize_column_order(out, style = "easystats")

  # in case data grid contained column names that are reserved words,
  # rename those back now...
  colnames(out) <- gsub("#####$", "", colnames(out))

  # remove and reorder some columns
  out$rowid <- out$Type <- out$rowid_dedup <- NULL

  # find at-variables
  at_variables <- attributes(model)$newdata_at
  if (is.null(at_variables)) {
    at_variables <- attributes(model)$by
  }

  # find cofficient name - differs for Bayesian models
  coef_name <- intersect(c("Predicted", "Coefficient"), colnames(out))[1]
  if (!is.null(at_variables) && !is.na(coef_name) && all(at_variables %in% colnames(out))) {
    out <- datawizard::data_relocate(
      out,
      select = at_variables,
      after = coef_name
    )
  }

  # extract response, remove from data frame
  reg_model <- attributes(model)$model
  if (!is.null(reg_model) && insight::is_model(reg_model)) {
    resp <- insight::find_response(reg_model)
    # check if response could be extracted
    if (!is.null(resp)) {
      # for some models, like brms-special response formula, we have multiple
      # values in "resp", so we iterate all of them separately
      for (r in resp) {
        out[[r]] <- NULL
      }
    }
  }

  out <- .safe(
    .add_model_parameters_attributes(
      out,
      model = model,
      ci = ci,
      exponentiate = exponentiate,
      verbose = verbose,
      ...
    ),
    out
  )

  attr(out, "object_name") <- insight::safe_deparse_symbol(substitute(model))
  attr(out, "coefficient_name") <- "Predicted"
  attr(out, "no_caption") <- TRUE

  # exponentiate coefficients and SE/CI, if requested
  out <- .exponentiate_parameters(out, model = NULL, exponentiate)

  class(out) <- c("parameters_model", "see_parameters_model", class(out))
  out
}


.rename_reserved_marginaleffects <- function(model) {
  # get focal terms - we might escape column names where focal terms
  # equal "reserved" names, like t- or z-statistic
  focal_terms <- attributes(model)$focal_terms
  reserved <- c("t", "z")
  renamed_focal <- NULL

  # any focal terms equals reserved words? if so, rename
  if (any(reserved %in% focal_terms)) {
    renamed_focal <- focal_terms[focal_terms %in% reserved]
    model <- datawizard::data_rename(
      model,
      select = renamed_focal,
      replacement = paste0(renamed_focal, "#####")
    )
  }
  model
}
