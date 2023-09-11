#' Conversion between EFA results and CFA structure
#'
#' Enables a conversion between Exploratory Factor Analysis (EFA) and
#' Confirmatory Factor Analysis (CFA) `lavaan`-ready structure.
#'
#' @param model An EFA model (e.g., a `psych::fa` object).
#' @param names Vector containing dimension names.
#' @param max_per_dimension Maximum number of variables to keep per dimension.
#' @inheritParams principal_components
#'
#' @examples
#' \donttest{
#' library(parameters)
#' if (require("psych") && require("lavaan")) {
#'   efa <- psych::fa(attitude, nfactors = 3)
#'
#'   model1 <- efa_to_cfa(efa)
#'   model2 <- efa_to_cfa(efa, threshold = 0.3)
#'   model3 <- efa_to_cfa(efa, max_per_dimension = 2)
#'
#'   suppressWarnings(anova(
#'     lavaan::cfa(model1, data = attitude),
#'     lavaan::cfa(model2, data = attitude),
#'     lavaan::cfa(model3, data = attitude)
#'   ))
#' }
#' }
#' @return Converted index.
#' @export
convert_efa_to_cfa <- function(model, ...) {
  UseMethod("convert_efa_to_cfa")
}



#' @rdname convert_efa_to_cfa
#' @inheritParams model_parameters.principal
#' @export
convert_efa_to_cfa.fa <- function(model,
                                  threshold = "max",
                                  names = NULL,
                                  max_per_dimension = NULL,
                                  ...) {
  .efa_to_cfa(model_parameters(model, threshold = threshold, ...),
              names = names,
              max_per_dimension = max_per_dimension,
              ...)
}

#' @export
convert_efa_to_cfa.fa.ci <- convert_efa_to_cfa.fa



#' @export
convert_efa_to_cfa.parameters_efa <- function(model,
                                              threshold = NULL,
                                              names = NULL,
                                              max_per_dimension = NULL,
                                              ...) {
  if (!is.null(threshold)) {
    model <- model_parameters(attributes(model)$model, threshold = threshold, ...)
  }

  .efa_to_cfa(model, names = names, max_per_dimension = max_per_dimension, ...)
}

#' @export
convert_efa_to_cfa.parameters_pca <- convert_efa_to_cfa.parameters_efa



#' @rdname convert_efa_to_cfa
#' @export
efa_to_cfa <- convert_efa_to_cfa




#' @keywords internal
.efa_to_cfa <- function(loadings, names = NULL, max_per_dimension=NULL, ...) {
  loadings <- attributes(loadings)$loadings_long

  # Get dimension names
  if (is.null(names)) {
    names <- unique(loadings$Component)
  }

  # Catch error
  if (length(names) != insight::n_unique(loadings$Component)) {
    insight::format_error(
      paste0(
        "The `names` vector must be of same length as the number of dimensions, in this case ",
        length(unique(loadings$Component)),
        "."
      )
    )
  }

  cfa <- NULL
  # Iterate over dimensions
  for (i in seq_along(names)) {

    # Find correct subset
    items <- loadings[loadings$Component == unique(loadings$Component)[i], ]

    # Find corresponding items
    items <- as.character(loadings[loadings$Component == unique(loadings$Component)[i], "Variable"])

    # Subset if need be to keep only a certain number
    if (!is.null(max_per_dimension) && max_per_dimension > 0) {
      items <- as.character(stats::na.omit(items[1:max_per_dimension]))
    }

    # Append that list
    cfa <- c(cfa, paste0(names[i], " =~ ", paste(items, collapse = " + ")))
  }

  cfa <- paste0(cfa, collapse = "\n")
  cfa <- paste0("# Latent variables\n", cfa)
  class(cfa) <- c("cfa_model", class(cfa))
  cfa
}

#' @export
print.cfa_model <- function(x, ...) {
  cat(x)
  invisible(x)
}
