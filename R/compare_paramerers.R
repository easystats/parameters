#' @title Compare model parameters of multiple models
#' @name compare_parameters
#'
#' @description Compute and extract model parameters of multiple regression models.
#'   See \code{\link{model_parameters}} for further details.
#'
#' @param ... One or more regression model objects. Regression models may be
#'   of different model types.
#' @param component Model component for which parameters should be shown. See
#'   documentation for related model class in \code{\link{model_parameters}}.
#' @inheritParams model_parameters.default
#' @inheritParams model_parameters.cpglmm
#'
#' @return A data frame of indices related to the model's parameters.
#'
#' @examples
#' data(iris)
#' lm1 <- lm(Sepal.Length ~ Species, data = iris)
#' lm2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
#' lm3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
#' compare_parameters(lm1, lm2, lm3)
#'
#' @importFrom insight is_model_supported
#' @export
compare_parameters <- function(..., ci = .95, effects = "fixed", component = "all", standardize = NULL, exponentiate = FALSE, df_method = NULL, p_adjust = NULL, verbose = TRUE) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  m <- mapply(function(.x, .y) {
    # model parameters
    dat <- model_parameters(.x, ci = ci, effects = effects, component = component, standardize = standardize, exponentiate = exponentiate, df_method = df_method, p_adjust = p_adjust, verbose = verbose)
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
    # add suffix
    ignore <- colnames(mop) %in% c("Parameter", "Component")
    colnames(dat)[!ignore] <- paste0(colnames(dat)[!ignore], ".", .y)
    dat
  }, objects, object_names, SIMPLIFY = FALSE)

  model_names <- gsub("\"", "", .safe_deparse(object_names), fixed = TRUE)
  all_models <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component")), m)

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
  x
}
