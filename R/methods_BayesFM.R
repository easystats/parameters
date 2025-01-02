#' Parameters from Bayesian Exploratory Factor Analysis
#'
#' Format Bayesian Exploratory Factor Analysis objects from the BayesFM package.
#'
#' @param model Bayesian EFA created by the `BayesFM::befa`.
#' @inheritParams principal_components
#' @inheritParams bayestestR::describe_posterior
#' @inheritParams model_parameters.default
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' library(parameters)
#' \donttest{
#' if (require("BayesFM")) {
#'   efa <- BayesFM::befa(mtcars, iter = 1000)
#'   results <- model_parameters(efa, sort = TRUE, verbose = FALSE)
#'   results
#'   efa_to_cfa(results, verbose = FALSE)
#' }
#' }
#' @return A data frame of loadings.
#' @export
model_parameters.befa <- function(model,
                                  sort = FALSE,
                                  centrality = "median",
                                  dispersion = FALSE,
                                  ci = 0.95,
                                  ci_method = "eti",
                                  test = NULL,
                                  verbose = TRUE,
                                  ...) {
  if (!attr(model, "post.column.switch") || !attr(model, "post.sign.switch")) {
    insight::check_if_installed("BayesFM")
    if (!attr(model, "post.column.switch")) model <- BayesFM::post.column.switch(model)
    if (!attr(model, "post.sign.switch")) model <- BayesFM::post.sign.switch(model)
  }

  factor_loadings <- as.data.frame(model$alpha)
  names(factor_loadings) <- gsub("alpha:", "", names(factor_loadings), fixed = TRUE)
  factor_loadings <- stats::reshape(
    factor_loadings,
    direction = "long",
    varying = list(names(factor_loadings)),
    sep = "_",
    timevar = "Variable",
    v.names = "Loading",
    idvar = "Draw",
    times = names(factor_loadings)
  )

  components <- as.data.frame(model$dedic)
  names(components) <- gsub("dedic:", "", names(components), fixed = TRUE)
  components <- stats::reshape(
    components,
    direction = "long",
    varying = list(names(components)),
    sep = "_",
    timevar = "Variable",
    v.names = "Component",
    idvar = "Draw",
    times = names(components)
  )

  factor_loadings <- merge(components, factor_loadings)

  # Compute posterior by dedic
  long_loadings <- data.frame()
  for (var in unique(factor_loadings$Variable)) {
    for (comp in unique(factor_loadings$Component)) {
      chunk <- factor_loadings[factor_loadings$Variable == var & factor_loadings$Component == comp, ] # nolint
      if (nrow(chunk) == 0) {
        rez <-
          bayestestR::describe_posterior(
            factor_loadings$Loading,
            centrality = centrality,
            dispersion = dispersion,
            ci = ci,
            ci_method = ci_method,
            test = test,
            verbose = verbose,
            ...
          )
        rez[1, ] <- NA
      } else {
        rez <-
          bayestestR::describe_posterior(
            chunk$Loading,
            centrality = centrality,
            dispersion = dispersion,
            ci = ci,
            ci_method = ci_method,
            test = test,
            verbose = verbose,
            ...
          )
      }
      long_loadings <- rbind(
        long_loadings,
        cbind(data.frame(Component = comp, Variable = var), rez)
      )
    }
  }
  long_loadings$Component <- paste0("F", long_loadings$Component)

  # Clean
  long_loadings$Parameter <- NULL
  if ("CI" %in% names(long_loadings) && insight::n_unique(long_loadings$CI) == 1) {
    long_loadings$CI <- NULL
  }
  long_loadings <- long_loadings[long_loadings$Component != 0, ]

  factor_loadings <- .wide_loadings(
    long_loadings,
    loadings_columns = names(long_loadings)[3],
    component_column = "Component",
    variable_column = "Variable"
  )

  # Add attributes
  attr(factor_loadings, "model") <- model
  attr(factor_loadings, "additional_arguments") <- list(...)
  attr(factor_loadings, "n") <- insight::n_unique(long_loadings$Component)
  attr(factor_loadings, "loadings_columns") <- names(factor_loadings)[2:ncol(factor_loadings)]
  attr(factor_loadings, "ci") <- ci

  # Sorting
  if (isTRUE(sort)) {
    factor_loadings <- .sort_loadings(factor_loadings)
  }


  # Add some more attributes
  long_loadings <- stats::na.omit(long_loadings)
  row.names(long_loadings) <- NULL
  attr(factor_loadings, "loadings_long") <- long_loadings

  # add class-attribute for printing
  class(factor_loadings) <- c("parameters_efa", class(factor_loadings))

  factor_loadings
}
