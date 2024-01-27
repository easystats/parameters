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
#' @param ci_method Method for computing degrees of freedom for p-values
#'   and confidence intervals (CI). See documentation for related model class
#'   in [model_parameters()].
#' @param coefficient_names Character vector with strings that should be used
#'   as column headers for the coefficient column. Must be of same length as
#'   number of models in `...`, or length 1. If length 1, this name will be
#'   used for all coefficient columns. If `NULL`, the name for the coefficient
#'   column will detected automatically (as in `model_parameters()`).
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
#' @examplesIf require("gt", quietly = TRUE)
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' compare_parameters(lm1, lm2)
#'
#' # custom style
#' compare_parameters(lm1, lm2, select = "{estimate}{stars} ({se})")
#'
#' \donttest{
#' # custom style, in HTML
#' result <- compare_parameters(lm1, lm2, select = "{estimate}<br>({se})|{p}")
#' print_html(result)
#' }
#'
#' data(mtcars)
#' m1 <- lm(mpg ~ wt, data = mtcars)
#' m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' compare_parameters(m1, m2)
#' \donttest{
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
                               ci = 0.95,
                               effects = "fixed",
                               component = "conditional",
                               standardize = NULL,
                               exponentiate = FALSE,
                               ci_method = "wald",
                               p_adjust = NULL,
                               select = NULL,
                               column_names = NULL,
                               pretty_names = TRUE,
                               coefficient_names = NULL,
                               keep = NULL,
                               drop = NULL,
                               verbose = TRUE) {
  models <- list(...)

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
    model_names <- match.call(expand.dots = FALSE)[["..."]]
    if (length(names(model_names)) > 0) {
      model_names <- names(model_names)
    } else if (any(vapply(model_names, is.call, TRUE))) {
      model_names <- paste("Model", seq_along(models), sep = " ")
    } else {
      model_names <- vapply(model_names, as.character, character(1))
      names(models) <- model_names
    }
  }

  supported_models <- vapply(models, function(i) {
    insight::is_model_supported(i) || inherits(i, "lavaan") || inherits(i, "parameters_model")
  }, TRUE)

  if (!all(supported_models)) {
    if (verbose) {
      insight::format_alert(
        sprintf("Following objects are not supported: %s", toString(model_names[!supported_models])),
        "Dropping unsupported models now."
      )
    }
    models <- models[supported_models]
    model_names <- model_names[supported_models]
  }

  # set default
  if (is.null(select)) {
    if (is.null(ci) || is.na(ci)) {
      # if user set CI to NULL, show only estimates by default
      select <- "{estimate}"
    } else {
      # if we have CI, include them
      select <- "ci"
    }
  }

  # provide own names
  if (!is.null(column_names)) {
    if (length(column_names) != length(model_names)) {
      if (isTRUE(verbose)) {
        insight::format_alert("Number of column names does not match number of models.")
      }
    } else {
      model_names <- column_names
    }
  }

  # make sure we have enough coefficient names - else, repeat first value
  if (!is.null(coefficient_names) && length(coefficient_names) < length(models)) {
    coefficient_names <- rep(coefficient_names[1], length(models))
  }

  # iterate all models and create list of model parameters
  m <- lapply(seq_along(models), function(i) {
    model <- models[[i]]
    model_name <- model_names[[i]]

    if (inherits(model, "parameters_model")) {
      # we already have model parameters object...
      dat <- model
    } else {
      # set default-ci_type for Bayesian models
      if (.is_bayesian_model(model) && !ci_method %in% c("hdi", "quantile", "ci", "eti", "si", "bci", "bcai")) {
        ci_method_tmp <- "eti"
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
        wb_component = FALSE,
        verbose = verbose
      )
    }

    # set specific names for coefficient column
    coef_name <- attributes(dat)$coefficient_name
    if (!is.null(coef_name) && is.null(coefficient_names)) {
      colnames(dat)[colnames(dat) == "Coefficient"] <- coef_name
    } else if (!is.null(coefficient_names)) {
      colnames(dat)[colnames(dat) == "Coefficient"] <- coefficient_names[i]
    }

    # set pretty parameter names
    dat <- .set_pretty_names(dat, pretty_names)

    # make sure we have a component- and effects column, for merging
    if (!"Component" %in% colnames(dat)) {
      dat$Component <- "conditional"
    }
    if (!"Effects" %in% colnames(dat)) {
      dat$Effects <- "fixed"
    }
    if (!"Group" %in% colnames(dat)) {
      dat$Group <- ""
    }

    # add zi-suffix to parameter names
    if (any(dat$Component == "zero_inflated")) {
      dat$Parameter[dat$Component == "zero_inflated"] <- paste0(dat$Parameter[dat$Component == "zero_inflated"], " (zi)")
    }

    # add suffix
    ignore <- colnames(dat) %in% c("Parameter", "Component", "Effects", "Group")
    colnames(dat)[!ignore] <- paste0(colnames(dat)[!ignore], ".", model_name)

    # save model number, for sorting
    dat$model <- i
    dat$model[.in_intercepts(dat$Parameter)] <- 0

    dat
  })

  object_attributes <- lapply(m, attributes)
  names(object_attributes) <- model_names

  # merge all data frames
  all_models <- suppressWarnings(Reduce(function(x, y) {
    merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component", "Effects", "Group"))
  }, m))

  # find columns with model numbers and create new variable "params_order",
  # which is pasted together of all model-column indices. Take lowest index of
  # all model-column indices, which then indicates order of parameters/rows.
  model_cols <- which(startsWith(colnames(all_models), "model"))
  params_order <- as.numeric(substr(gsub("NA", "", do.call(paste0, all_models[model_cols]), fixed = TRUE), 0, 1))

  all_models <- all_models[order(params_order), ]
  all_models[model_cols] <- NULL

  # remove empty group-column
  if (!any(nzchar(as.character(all_models$Group), keepNA = TRUE))) {
    all_models$Group <- NULL
  }

  attr(all_models, "model_names") <- gsub("\"", "", unlist(lapply(model_names, insight::safe_deparse)), fixed = TRUE)
  attr(all_models, "output_style") <- select
  attr(all_models, "all_attributes") <- object_attributes
  class(all_models) <- c("compare_parameters", "see_compare_parameters", unique(class(all_models)))

  all_models
}


#' @rdname compare_parameters
#' @export
compare_models <- compare_parameters


# helper ----------------------------


.set_pretty_names <- function(x, pretty_names) {
  # check if pretty names should be replaced by value labels
  # (if we have labelled data)
  if (isTRUE(getOption("parameters_labels", FALSE)) || identical(pretty_names, "labels")) {
    attr(x, "pretty_names") <- attr(x, "pretty_labels", exact = TRUE)
    pretty_names <- TRUE
  }

  att <- attributes(x)

  if (!is.null(att$pretty_names)) {
    # remove strings with NA names
    att$pretty_names <- att$pretty_names[!is.na(names(att$pretty_names))]
    if (length(att$pretty_names) != length(x$Parameter)) {
      match_pretty_names <- match(names(att$pretty_names), x$Parameter)
      match_pretty_names <- match_pretty_names[!is.na(match_pretty_names)]
      if (length(match_pretty_names)) {
        x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
      }
    } else {
      match_pretty_names <- att$pretty_names[x$Parameter]
      if (anyNA(match_pretty_names)) {
        match_pretty_names <- match(names(att$pretty_names), x$Parameter)
        match_pretty_names <- match_pretty_names[!is.na(match_pretty_names)]
        if (length(match_pretty_names)) {
          x$Parameter[match_pretty_names] <- att$pretty_names[x$Parameter[match_pretty_names]]
        }
      } else {
        x$Parameter <- att$pretty_names[x$Parameter]
      }
    }
  }

  if (!is.null(x$Parameter)) {
    x$Parameter <- gsub("]", ")", gsub("[", "(", x$Parameter, fixed = TRUE), fixed = TRUE)
  }

  x
}
