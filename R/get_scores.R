#' Get Scores from Principal Component Analysis (PCA)
#'
#' \code{get_scores()} takes \code{n_items} amount of items that load the most (either by loading cutoff or number) on a component, and then computes their average.
#'
#' @param x An object returned by \code{\link{principal_components}}.
#' @param n_items Number of required (i.e. non-missing) items to build the sum score. If \code{NULL}, the value is chosen to match half of the number of columns in a data frame.
#'
#' @details \code{get_scores()} takes the results from \code{\link{principal_components}}
#'   and extracts the variables for each component found by the PCA. Then, for
#'   each of these "subscales", row means are calculated (which equals adding
#'   up the single items and dividing by the number of items). This results in
#'   a sum score for each component from the PCA, which is on the same scale as
#'   the original, single items that were used to compute the PCA.
#'
#' @examples
#' library(parameters)
#' pca <- principal_components(mtcars[, 1:7], n = 2, rotation = "varimax")
#'
#' # PCA extracted two components
#' pca
#'
#' # assignment of items to each component
#' closest_component(pca)
#'
#' # now we want to have sum scores for each component
#' get_scores(pca)
#'
#' # compare to manually computed sum score for 2nd component, which
#' # consists of items "hp" and "qsec"
#' (mtcars$hp + mtcars$qsec) / 2
#' @return A data frame with subscales, which are average sum scores for all items from each component.
#' @export
get_scores <- function(x, n_items = NULL) {
  subscales <- closest_component(x)
  data_set <- attributes(x)$data_set

  out <- lapply(sort(unique(subscales)), function(.subscale) {
    columns <- names(subscales)[subscales == .subscale]
    items <- data_set[columns]

    if (is.null(n_items)) {
      .n_items <- round(ncol(items) / 2)
    } else {
      .n_items <- n_items
    }

    apply(items, 1, function(i) ifelse(sum(!is.na(i)) >= .n_items, mean(i, na.rm = TRUE), NA))
  })

  out <- as.data.frame(do.call(cbind, out))
  colnames(out) <- sprintf("Component_%i", 1:ncol(out))

  out
}
