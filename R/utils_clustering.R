

# #' @export
# print.cluster_analysis <- function(x, digits = 2, ...) {
#   # retrieve data
#   dat <- attr(x, "data", exact = TRUE)
#
#   if (is.null(dat)) {
#     stop("Could not find data frame that was used for cluster analysis.", call. = FALSE)
#   }
#
#   # save output from cluster_discrimination()
#   accuracy <- attributes(x)$accuracy
#
#   # headline
#   insight::print_color("# Cluster Analysis (mean z-score by cluster)\n\n", "blue")
#
#   # round numeric variables (i.e. all but first term column)
#   dat[2:ncol(dat)] <- sapply(dat[2:ncol(dat)], round, digits = digits)
#   print.data.frame(dat, row.names = FALSE)
#
#   if (!is.null(accuracy)) {
#     cat("\n")
#     print(accuracy)
#   }
#   invisible(x)
# }


# Utils -------------------------------------------------------------------

#' @keywords internal
.prepare_data_clustering <- function(x, include_factors = FALSE, standardize = FALSE, preprocess = TRUE, ...) {
  if (preprocess == FALSE) {
    return(x)
  }

  # Convert factors to numeric
  # include factors?
  if (include_factors) {
    # ordered factors to numeric
    factors <- sapply(x, is.ordered)
    if (any(factors)) {
      x[factors] <- sapply(x[factors], datawizard::convert_data_to_numeric)
    }

    # character and factors to dummies
    factors <- sapply(x, function(i) is.character(i) | is.factor(i))
    if (any(factors)) {
      dummies <- lapply(x[factors], .factor_to_dummy)
      x <- cbind(x[!factors], dummies)
    }
  } else {
    # remove factors
    x <- x[sapply(x, is.numeric)]
  }

  # Remove all missing values from data, only use numerics
  x <- stats::na.omit(x)
  if (standardize == TRUE) {
    x <- datawizard::standardize(x, ...)
  }
  x
}
