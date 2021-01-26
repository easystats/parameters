#' Reshape loadings between wide/long formats
#'
#' Reshape loadings between wide/long formats.
#'
#'
#' @examples
#' library(parameters)
#' library(psych)
#'
#' pca <- model_parameters(psych::fa(attitude, nfactors = 3))
#' loadings <- reshape_loadings(pca)
#'
#' loadings
#' reshape_loadings(loadings)
#' @export
reshape_loadings <- function(x, ...) {
  UseMethod("reshape_loadings")
}

#' @rdname reshape_loadings
#' @inheritParams principal_components
#' @export
reshape_loadings.parameters_efa <- function(x, threshold = NULL, ...) {
  current_format <- attributes(x)$loadings_format

  if (is.null(current_format) || current_format == "wide") {
    .long_loadings(x, threshold = threshold)
  } else {
    .wide_loadings(x)
  }
}


#' @rdname reshape_loadings
#' @param loadings_columns Vector indicating the columns corresponding to loadings.
#' @export
reshape_loadings.data.frame <- function(x, threshold = NULL, loadings_columns = NULL, ...) {
  if (is.null(loadings_columns)) loadings_columns <- 1:ncol(x)
  if (length(loadings_columns) > 1) {
    .long_loadings(x, threshold = threshold, loadings_columns = loadings_columns)
  }
}



#' @importFrom insight export_table format_table
#' @importFrom stats reshape
#' @keywords internal
.wide_loadings <- function(loadings,
                           loadings_columns = "Loading",
                           component_column = "Component",
                           variable_column = "Variable",
                           ...) {
  if (is.numeric(loadings[[component_column]])) {
    loadings[[component_column]] <- paste0("F", loadings[[component_column]])
  }

  complexity_column <- if ("Complexity" %in% colnames(loadings)) "Complexity" else NULL
  uniqueness_column <- if ("Uniqueness" %in% colnames(loadings)) "Uniqueness" else NULL

  reshape_columns <- c(loadings_columns, component_column, variable_column, complexity_column, uniqueness_column)

  loadings <- stats::reshape(
    loadings[reshape_columns],
    idvar = variable_column,
    timevar = component_column,
    direction = "wide",
    v.names = c(loadings_columns),
    sep = "_"
  )
  names(loadings) <- gsub(paste0(loadings_columns, "_"), "", names(loadings))
  attr(loadings, "loadings_format") <- "wide"
  class(loadings) <- unique(c("parameters_loadings", class(loadings)))

  # clean-up, column-order
  row.names(loadings) <- NULL
  column_order <- c(setdiff(colnames(loadings), c("Complexity", "Uniqueness")), c("Complexity", "Uniqueness"))
  loadings[column_order[column_order %in% colnames(loadings)]]
}


#' @importFrom stats reshape
#' @keywords internal
.long_loadings <- function(loadings, threshold = NULL, loadings_columns = NULL) {
  if (is.null(loadings_columns)) {
    loadings_columns <- attributes(loadings)$loadings_columns
  }


  if (!is.null(threshold)) {
    loadings <- .filter_loadings(loadings, threshold = threshold, loadings_columns = loadings_columns)
  }

  # Reshape to long
  long <- stats::reshape(loadings,
    direction = "long",
    varying = list(names(loadings)[loadings_columns]),
    v.names = "Loading",
    timevar = "Component",
    idvar = "Variable"
  )

  # Restore component names
  for (i in 1:.n_unique(long$Component)) {
    component <- unique(long$Component)[[i]]
    name <- names(loadings)[loadings_columns][[i]]
    long[long$Component == component, "Component"] <- name
  }

  # Filtering
  long <- long[!is.na(long$Loading), ]

  row.names(long) <- NULL
  # Reorder columns
  loadings <- long[, c(
    "Component",
    "Variable",
    "Loading",
    names(loadings)[-loadings_columns][!names(loadings)[-loadings_columns] %in% c("Component", "Variable", "Loading")]
  )]

  attr(loadings, "loadings_format") <- "long"
  class(loadings) <- unique(c("parameters_loadings", class(loadings)))
  loadings
}





#' @importFrom insight export_table
#' @export
print.parameters_loadings <- function(x, ...) {
  formatted_table <- insight::format_table(x)
  cat(insight::export_table(formatted_table))
  invisible(x)
}
