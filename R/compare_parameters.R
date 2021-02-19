#' @title Compare model parameters of multiple models
#' @name compare_parameters
#'
#' @description Compute and extract model parameters of multiple regression models.
#'   See \code{\link{model_parameters}} for further details.
#'
#' @param ... One or more regression model objects, or objects returned by
#'   \code{model_parameters()}. Regression models may be of different model
#'   types.
#' @param component Model component for which parameters should be shown. See
#'   documentation for related model class in \code{\link{model_parameters}}.
#' @param column_names Character vector with strings that should be used as
#'   column headers. Must be of same length as number of models in \code{...}.
#' @param style String, indicating which style of output is requested. Following
#'   templates are possible:
#'   \itemize{
#'     \item \code{"ci"}: Estimate and confidence intervals, no asterisks for p-values.
#'     \item \code{"se"}: Estimate and standard errors, no asterisks for p-values.
#'     \item \code{"ci_p"}: Estimate, confidence intervals and asterisks for p-values.
#'     \item \code{"se_p"}: Estimate, standard errors and asterisks for p-values.
#'     \item \code{"ci_p2"}: Estimate, confidence intervals and numeric p-values, in two columns.
#'     \item \code{"se_p2"}: Estimate, standard errors and numeric p-values, in two columns.
#'   }
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.cpglmm
#'
#' @note This function is in an early stage and does not yet cope with more
#'   complex models, and probably does not yet properly render all model
#'   components.
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' if (packageVersion("insight") >= "0.13.0") {
#'   data(iris)
#'   lm1 <- lm(Sepal.Length ~ Species, data = iris)
#'   lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#'   lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#'   compare_parameters(lm1, lm2, lm3)
#'
#'   data(mtcars)
#'   m1 <- lm(mpg ~ wt, data = mtcars)
#'   m2 <- glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#'   compare_parameters(m1, m2)
#'
#'   # exponentiate coefficients, but not for lm
#'   compare_parameters(m1, m2, exponentiate = "nongaussian")
#'
#'   # change column names
#'   compare_parameters(m1, m2, column_names = c("linear model", "logistic reg."))
#' }
#' @importFrom insight is_model_supported
#' @export
compare_parameters <- function(..., ci = .95, effects = "fixed", component = "conditional", standardize = NULL, exponentiate = FALSE, df_method = "wald", p_adjust = NULL, style = NULL, column_names = NULL, verbose = TRUE) {
  models <- list(...)
  model_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(models, function(i) insight::is_model_supported(i) | inherits(i, "lavaan") | inherits(i, "parameters_model"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(model_names[!supported_models], collapse = ", ")))
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
    # model parameters
    if (inherits(model, "parameters_model")) {
      dat <- model
    } else {
      dat <- model_parameters(model, ci = ci, effects = effects, component = component, standardize = standardize, exponentiate = exponentiate, df_method = df_method, p_adjust = p_adjust, verbose = verbose)
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
    dat
  })

  object_attributes <- lapply(m, attributes)
  names(object_attributes) <- model_names

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
  x$Parameter <- gsub("]", ")", gsub("[", "(", x$Parameter, fixed = TRUE), fixed = TRUE)
  x
}
