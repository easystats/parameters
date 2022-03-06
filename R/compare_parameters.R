#' @title Compare model parameters of multiple models
#' @name compare_parameters
#'
#' @description Compute and extract model parameters of multiple regression
#'   models. See [model_parameters()] for further details.
#'
#' @param ... One or more regression model objects, or objects returned by
#'   `model_parameters()`. Regression models may be of different model
#'   types. Model objects may be passed comma separated, or as a list.
#'   If model objects are passed with names or the list has named elements,
#'   these names will be used as column names.
#' @param component Model component for which parameters should be shown. See
#'   documentation for related model class in [model_parameters()].
#' @param column_names Character vector with strings that should be used as
#'   column headers. Must be of same length as number of models in `...`.
#' @param ci_method Method for computing degrees of freedom for p values
#'   and confidence intervals (CI). See documentation for related model class
#'   in [model_parameters()].
#' @param style String, indicating which style of output is requested. Following
#'   templates are possible:
#'
#'  - `"ci"`: Estimate and confidence intervals, no asterisks for p-values.
#'  - `"se"`: Estimate and standard errors, no asterisks for p-values.
#'  - `"ci_p"`: Estimate, confidence intervals and asterisks for p-values.
#'  - `"se_p"`: Estimate, standard errors and asterisks for p-values.
#'  - `"ci_p2"`: Estimate, confidence intervals and numeric p-values, in two columns.
#'  - `"se_p2"`: Estimate, standard errors and numeric p-values, in two columns.
#' @param keep,drop Character containing a regular expression pattern that
#'   describes the parameters that should be included (for `keep`) or excluded
#'   (for `drop`) in the returned data frame. `keep` may also be a
#'   named list of regular expressions. All non-matching parameters will be
#'   removed from the output. If `keep` is a character vector, every parameter
#'   name in the *"Parameter"* column that matches the regular expression in
#'   `keep` will be selected from the returned data frame (and vice versa,
#'   all parameter names matching `drop` will be excluded). Furthermore, if
#'   `keep` has more than one element, these will be merged with an `OR`
#'   operator into a regular expression pattern like this: `"(one|two|three)"`.
#'   If `keep` is a named list of regular expression patterns, the names of the
#'   list-element should equal the column name where selection should be
#'   applied. This is useful for model objects where `model_parameters()`
#'   returns multiple columns with parameter components, like in
#'   [model_parameters.lavaan()]. Note that the regular expression pattern
#'   should match the parameter names as they are stored in the returned data
#'   frame, which can be different from how they are printed. Inspect the
#'   `$Parameter` column of the parameters table to get the exact parameter
#'   names.
#'
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.cpglmm
#' @inheritParams print.parameters_model
#'
#' @details
#'
#' This function is in an early stage and does not yet cope with more complex
#' models, and probably does not yet properly render all model components. It
#' should also be noted that when including models with interaction terms, not
#' only do the values of the parameters change, but so does their meaning (from
#' main effects, to simple slopes), thereby making such comparisons hard.
#' Therefore, you should not use this function to compare models with
#' interaction terms with models without interaction terms.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' compare_parameters(lm1, lm2)
#'
#' data(mtcars)
#' m1 <- lm(mpg ~ wt, data = mtcars)
#' m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' compare_parameters(m1, m2)
#' \dontrun{
#' # exponentiate coefficients, but not for lm
#' compare_parameters(m1, m2, exponentiate = "nongaussian")
#'
#' # change column names
#' compare_parameters("linear model" = m1, "logistic reg." = m2)
#' compare_parameters(m1, m2, column_names = c("linear model", "logistic reg."))
#'
#' # or as list
#' compare_parameters(list(m1, m2))
#' compare_parameters(list("linear model" = m1, "logistic reg." = m2))
#' }
#' @export
compare_parameters <- function(...,
                               ci = .95,
                               effects = "fixed",
                               component = "conditional",
                               standardize = NULL,
                               exponentiate = FALSE,
                               ci_method = "wald",
                               p_adjust = NULL,
                               style = NULL,
                               column_names = NULL,
                               keep = NULL,
                               drop = NULL,
                               parameters = keep,
                               verbose = TRUE,
                               df_method = ci_method) {
  models <- list(...)

  ## TODO remove later
  if (!missing(df_method) && !identical(ci_method, df_method)) {
    warning(insight::format_message("Argument 'df_method' is deprecated. Please use 'ci_method' instead."), call. = FALSE)
    ci_method <- df_method
  }

  if (length(models) == 1) {
    if (insight::is_model(models[[1]]) || inherits(models[[1]], "parameters_model")) {
      modellist <- FALSE
    } else {
      models <- models[[1]]
      modellist <- TRUE
    }
  } else {
    modellist <- FALSE
  }

  if (isTRUE(modellist)) {
    model_names <- names(models)
    if (length(model_names) == 0) {
      model_names <- paste("Model", seq_along(models), sep = " ")
      names(models) <- model_names
    }
  } else {
    model_names <- match.call(expand.dots = FALSE)$`...`
    if (length(names(model_names)) > 0) {
      model_names <- names(model_names)
    } else if (any(sapply(model_names, is.call))) {
      model_names <- paste("Model", seq_along(models), sep = " ")
    } else {
      model_names <- sapply(model_names, as.character)
      names(models) <- model_names
    }
  }

  supported_models <- sapply(models, function(i) insight::is_model_supported(i) | inherits(i, "lavaan") | inherits(i, "parameters_model"))

  if (!all(supported_models)) {
    warning(insight::format_message(
      sprintf("Following objects are not supported: %s", paste0(model_names[!supported_models], collapse = ", ")),
      "Dropping unsupported models now."
    ), call. = FALSE)
    models <- models[supported_models]
    model_names <- model_names[supported_models]
  }

  # set default
  if (is.null(style)) {
    style <- "ci"
  }

  # provide own names
  if (!is.null(column_names)) {
    if (length(column_names) != length(model_names)) {
      if (isTRUE(verbose)) {
        warning("Number of column names does not match number of models.", call. = FALSE)
      }
    } else {
      model_names <- column_names
    }
  }

  # iterate all models and create list of model parameters
  m <- lapply(1:length(models), function(i) {
    model <- models[[i]]
    model_name <- model_names[[i]]

    if (inherits(model, "parameters_model")) {

      # we already have model parameters object...
      dat <- model
    } else {

      # set default-ci_type for Bayesian models
      if (.is_bayesian_model(model) && !ci_method %in% c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai")) {
        ci_method_tmp <- "hdi"
      } else {
        ci_method_tmp <- ci_method
      }

      # here we have a model object that needs to be passed to model_parameters
      dat <- model_parameters(
        model,
        ci = ci,
        effects = effects,
        component = component,
        standardize = standardize,
        exponentiate = exponentiate,
        ci_method = ci_method_tmp,
        p_adjust = p_adjust,
        keep = keep,
        drop = drop,
        verbose = verbose
      )
    }
    # set specific names for coefficient column
    coef_name <- attributes(dat)$coefficient_name
    if (!is.null(coef_name)) {
      colnames(dat)[colnames(dat) == "Coefficient"] <- coef_name
    }
    # set pretty parameter names
    dat <- .set_pretty_names(dat)
    # make sure we have a component-column, for merging
    if (!"Component" %in% colnames(dat)) {
      dat$Component <- "conditional"
    }
    # add zi-suffix to parameter names
    if (any(dat$Component == "zero_inflated")) {
      dat$Parameter[dat$Component == "zero_inflated"] <- paste0(dat$Parameter[dat$Component == "zero_inflated"], " (zi)")
    }
    # add suffix
    ignore <- colnames(dat) %in% c("Parameter", "Component")
    colnames(dat)[!ignore] <- paste0(colnames(dat)[!ignore], ".", model_name)
    # save model number, for sorting
    dat$model <- i
    dat$model[.in_intercepts(dat$Parameter)] <- 0

    ## NOT SURE why we needed this? It duplicates parameters when
    ## these are in different order in different models

    # add index for row order. needed later, because "merge()" sometimes
    # messes up the correct row sorting, despite setting "sort = FALSE"
    # dat$.row_index <- 1:nrow(dat)

    dat
  })

  object_attributes <- lapply(m, attributes)
  names(object_attributes) <- model_names

  ## NOT SURE why we needed this? It duplicates parameters when
  ## these are in different order in different models

  # # merge all data frames
  # all_models <- suppressWarnings(Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component", ".row_index")), m))
  #
  # # fix row order
  # row_order <- order(all_models$.row_index)
  # all_models <- all_models[row_order, ]
  # all_models$.row_index <- NULL

  # merge all data frames
  all_models <- suppressWarnings(Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component")), m))

  # find columns with model numbers and create new variable "params_order",
  # which is pasted together of all model-column indices. Take lowest index of
  # all model-column indices, which then indicates order of parameters/rows.
  model_cols <- which(grepl("^model", colnames(all_models)))
  params_order <- as.numeric(substr(gsub("NA", "", do.call(paste0, all_models[model_cols]), fixed = TRUE), 0, 1))

  all_models <- all_models[order(params_order), ]
  all_models[model_cols] <- NULL

  attr(all_models, "model_names") <- gsub("\"", "", unlist(lapply(model_names, .safe_deparse)), fixed = TRUE)
  attr(all_models, "output_style") <- style
  attr(all_models, "all_attributes") <- object_attributes
  class(all_models) <- c("compare_parameters", "see_compare_parameters", unique(class(all_models)))

  all_models
}


#' @rdname compare_parameters
#' @export
compare_models <- compare_parameters





# helper ----------------------------


.set_pretty_names <- function(x) {
  att <- attributes(x)
  if (!is.null(att$pretty_names)) {
    # remove strings with NA names
    att$pretty_names <- att$pretty_names[!is.na(names(att$pretty_names))]
    if (length(att$pretty_names) != length(x$Parameter)) {
      match_pretty_names <- stats::na.omit(match(names(att$pretty_names), x$Parameter))
      if (length(match_pretty_names)) {
        x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
      }
    } else {
      match_pretty_names <- att$pretty_names[x$Parameter]
      if (!anyNA(match_pretty_names)) {
        x$Parameter <- att$pretty_names[x$Parameter]
      } else {
        match_pretty_names <- stats::na.omit(match(names(att$pretty_names), x$Parameter))
        if (length(match_pretty_names)) {
          x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
        }
      }
    }
  }

  if (!is.null(x$Parameter)) {
    x$Parameter <- gsub("]", ")", gsub("[", "(", x$Parameter, fixed = TRUE), fixed = TRUE)
  }
  x
}
