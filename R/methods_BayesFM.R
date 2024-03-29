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

  loadings <- as.data.frame(model$alpha)
  names(loadings) <- gsub("alpha:", "", names(loadings), fixed = TRUE)
  loadings <- stats::reshape(
    loadings,
    direction = "long",
    varying = list(names(loadings)),
    sep = "_",
    timevar = "Variable",
    v.names = "Loading",
    idvar = "Draw",
    times = names(loadings)
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

  loadings <- merge(components, loadings)

  # Compute posterior by dedic
  long_loadings <- data.frame()
  for (var in unique(loadings$Variable)) {
    for (comp in unique(loadings$Component)) {
      chunk <- loadings[loadings$Variable == var & loadings$Component == comp, ]
      if (nrow(chunk) == 0) {
        rez <-
          bayestestR::describe_posterior(
            loadings$Loading,
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

  loadings <- .wide_loadings(long_loadings, loadings_columns = names(long_loadings)[3], component_column = "Component", variable_column = "Variable")



  # Add attributes
  attr(loadings, "model") <- model
  attr(loadings, "additional_arguments") <- list(...)
  attr(loadings, "n") <- insight::n_unique(long_loadings$Component)
  attr(loadings, "loadings_columns") <- names(loadings)[2:ncol(loadings)]
  attr(loadings, "ci") <- ci

  # Sorting
  if (isTRUE(sort)) {
    loadings <- .sort_loadings(loadings)
  }


  # Add some more attributes
  long_loadings <- stats::na.omit(long_loadings)
  row.names(long_loadings) <- NULL
  attr(loadings, "loadings_long") <- long_loadings

  # add class-attribute for printing
  class(loadings) <- c("parameters_efa", class(loadings))

  loadings
}
