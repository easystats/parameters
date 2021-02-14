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
#' @param style String, indicating which style of output is requested. Following
#'   templates are possible:
#'   \itemize{
#'     \item \code{"ci_p"}: Estimate, confidence intervals and asterisks for p-values.
#'     \item \code{"se_p"}: Estimate, standard errors and asterisks for p-values.
#'     \item \code{"ci"}: Estimate and confidence intervals, no asterisks for p-values.
#'     \item \code{"se"}: Estimate and standard errors, no asterisks for p-values.
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
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' compare_parameters(lm1, lm2, lm3)
#' @importFrom insight is_model_supported
#' @export
compare_parameters <- function(..., ci = .95, effects = "fixed", component = "conditional", standardize = NULL, exponentiate = FALSE, df_method = NULL, p_adjust = NULL, verbose = TRUE, style = NULL) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan") | inherits(i, "parameters_model"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  # set default
  if (is.null(style)) {
    style <- "ci_p"
  }

  # iterate all models and create list of model parameters
  m <- mapply(function(.x, .y) {
    # model parameters
    if (inherits(.x, "parameters_model")) {
      dat <- .x
    } else {
      dat <- model_parameters(.x, ci = ci, effects = effects, component = component, standardize = standardize, exponentiate = exponentiate, df_method = df_method, p_adjust = p_adjust, verbose = verbose)
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
    colnames(dat)[!ignore] <- paste0(colnames(dat)[!ignore], ".", .y)
    dat
  }, objects, object_names, SIMPLIFY = FALSE)

  # tell user that exponentiate only applies to non-Gaussian...
  if (isTRUE(exponentiate)) {
    if (any(sapply(m, function(i) isTRUE(attributes(i)$linear_model))) && isTRUE(verbose)) {
      message("Coefficients for linear models were not exponentiated.")
    }
  }

  # merge all data frames
  all_models <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component")), m)

  attr(all_models, "model_names") <- gsub("\"", "", unlist(lapply(object_names, .safe_deparse)), fixed = TRUE)
  attr(all_models, "output_style") <- style
  class(all_models) <- c("compare_parameters", unique(class(all_models)))

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
