#' Reshape loadings between wide/long formats
#'
#' Reshape loadings between wide/long formats.
#'
#'
#' @examples
#' library(parameters)
#' \dontrun{
#' library(psych)
#'
#' pca <- model_parameters(psych::fa(attitude, nfactors = 3))
#' loadings <- reshape_loadings(pca)
#'
#' loadings
#' reshape_loadings(loadings)
#'
#' }
#' @export
reshape_loadings <- function(x, ...) {
  UseMethod("reshape_loadings")
}

#' @rdname reshape_loadings
#' @inheritParams principal_components
#' @export
reshape_loadings.parameters_efa <- function(x, threshold = NULL, ...) {
  .long_loadings(x, threshold = threshold)
}


#' @rdname reshape_loadings
#' @param loadings_columns Vector indicating the columns corresponding to loadings.
#' @export
reshape_loadings.data.frame <- function(x, threshold = NULL, loadings_columns = NULL, ...) {
  if(is.null(loadings_columns)) loadings_columns <- 1:ncol(x)
  if(length(loadings_columns) > 1){
    .long_loadings(x, threshold = threshold, loadings_columns = loadings_columns)
  }
}



#' @importFrom stats reshape
#' @keywords internal
.wide_loadings <- function(loadings, loadings_columns = "Loading", component_column = "Component", variable_column = "Variable", ...) {

  if(is.numeric(loadings[[component_column]])){
    loadings[[component_column]] <- paste0("F", loadings[[component_column]])
  }

  loadings <- stats::reshape(
    loadings[c(loadings_columns, component_column, variable_column)],
    idvar = variable_column,
    timevar = component_column,
    direction = "wide",
    v.names = c(loadings_columns),
    sep = "_"
  )
  row.names(loadings) <- NULL
  names(loadings) <- gsub(paste0(loadings_columns, "_"), "", names(loadings))
  class(loadings) <- c("parameters_loadings", class(loadings))
  loadings
}


#' @importFrom stats reshape
#' @keywords internal
.long_loadings <- function(loadings, threshold = NULL, loadings_columns = NULL) {
  if (is.null(loadings_columns)) {
    loadings_columns <- attributes(loadings)$loadings_columns
  }


  if (!is.null(threshold)) {
    loadings <- .filer_loadings(loadings, threshold = threshold, loadings_columns = loadings_columns)
  }

  # Reshape to long
  long <- reshape(loadings,
                  direction = "long",
                  varying = list(names(loadings)[loadings_columns]),
                  v.names = "Loading",
                  timevar = "Component",
                  idvar = "Variable"
  )

  # Restore component names
  for (i in 1:length(unique(long$Component))) {
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

  class(loadings) <- c("parameters_loadings", class(loadings))
  loadings
}



#' @keywords internal
.filer_loadings <- function(loadings, threshold = 0.2, loadings_columns = NULL) {
  if (is.null(loadings_columns)) {
    loadings_columns <- attributes(loadings)$loadings_columns
  }


  if (threshold == "max") {
    for (i in 1:nrow(loadings)) {
      maxi <- max(abs(loadings[i, loadings_columns, drop = FALSE]))
      loadings[i, loadings_columns][abs(loadings[i, loadings_columns]) < maxi] <- NA
    }
  } else {
    loadings[, loadings_columns][abs(loadings[, loadings_columns]) < threshold] <- NA
  }

  loadings
}




#' @export
print.parameters_loadings <- function(x, ...){
  formatted_table <- parameters_table(x)
  cat(format_table(formatted_table))
}