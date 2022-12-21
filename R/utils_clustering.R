# Utils -------------------------------------------------------------------

#' @keywords internal
.prepare_data_clustering <- function(x,
                                     include_factors = FALSE,
                                     standardize = FALSE,
                                     preprocess = TRUE,
                                     ...) {
  if (preprocess == FALSE) {
    return(x)
  }

  # include factors?
  if (include_factors) {
    # ordered factors to numeric
    factors <- vapply(x, is.ordered, logical(1))
    if (any(factors)) {
      x[factors] <- sapply(
        x[factors],
        datawizard::to_numeric,
        dummy_factors = FALSE,
        preserve_levels = TRUE
      )
    }

    # character and factors to dummies
    factors <- sapply(x, function(i) is.character(i) | is.factor(i))

    if (any(factors)) {
      dummies <- lapply(x[factors], .factor_to_dummy)
      x <- cbind(x[!factors], dummies)
    }
  } else {
    # remove factors
    x <- x[vapply(x, is.numeric, logical(1))]
  }

  # Remove all missing values from data, only use numerics
  x <- stats::na.omit(x)

  if (standardize == TRUE) {
    x <- datawizard::standardize(x, ...)
  }

  x
}
