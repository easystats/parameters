#' @title Compare model parameters of multiple models
#' @name compare_paramerers
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
compare_paramerers <- function(..., ci = .95, effects = "fixed", component = "all", standardize = NULL, exponentiate = FALSE, df_method = NULL, p_adjust = NULL, verbose = TRUE) {
  objects <- list(...)
  object_names <- match.call(expand.dots = FALSE)$`...`

  supported_models <- sapply(objects, function(i) insight::is_model_supported(i) | inherits(i, "lavaan"))

  if (!all(supported_models)) {
    warning(sprintf("Following objects are not supported: %s", paste0(object_names[!supported_models], collapse = ", ")))
    objects <- objects[supported_models]
    object_names <- object_names[supported_models]
  }

  m <- mapply(function(.x, .y) {
    dat <- model_parameters(.x, ci = ci, effects = effects, component = component, standardize = standardize, exponentiate = exponentiate, df_method = df_method, p_adjust = p_adjust, verbose = verbose)
    model_name <- gsub("\"", "", .safe_deparse(.y), fixed = TRUE)
    if (!"Component" %in% colnames(dat)) {
      dat$Component <- "conditional"
    }
    attr(dat, "model_name") <- model_name
    dat
  }, objects, object_names, SIMPLIFY = FALSE)

  all_models <- Reduce(function(x, y) merge(x, y, all = TRUE, sort = FALSE, by = c("Parameter", "Component")), m)
  all_models
}
