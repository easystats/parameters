#' @inherit standardize
#' @importFrom stats median mad
#' @export
standardize.numeric <- function(x, method = "mean", ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  x <- stats::na.omit(x)

  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    warning(paste0("Variable `", name, "` contains only one unique value and will not be standardized."))
    return(x)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2) {
    if (is.null(names(x))) {
      name <- deparse(substitute(x))
    } else {
      name <- names(x)
    }
    warning(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
  }

  if (method == "mean") {
    return(as.vector((x - mean(x)) / stats::sd(x)))
  } else if (method == "median") {
    return(as.vector((x - stats::median(x)) / stats::mad(x)))
  } else if (method == "2sd") {
    return(as.vector((x - mean(x)) / (2 * stats::sd(x))))
  }
}










#' @inherit standardize
#' @export
standardize.factor <- function(x, ...) {
  return(x)
}
#' @export
standardize.character <- standardize.factor




#' @inheritParams standardize
#' @export
standardize.grouped_df <- function(x, method = "mean", select = NULL, exclude = NULL, ...) {
  info <- attributes(x)
  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- standardize(
      x[rows, ],
      select = select,
      exclude = exclude,
      method = method,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @title Data Standardization
#'
#' @description Standardize (scale and reduce, Z-score) the data so that the
#'   values are expressed in terms of standard deviation (i.e., mean = 0, SD = 1)
#'   or Median Absolute Deviance (median = 0, MAD = 1). A \code{normalization}
#'   scales all numeric variables in the 0 - 1 range.
#'
#' @inheritParams standardize
#' @param select For a data frame, character vector of column names to be
#'   standardized. If \code{NULL} (the default), all variables will be
#'   standardized.
#' @param exclude For a data frame, character vector of column names to
#'   be excluded from standardization.
#'
#' @examples
#' summary(standardize(iris))
#' @export
standardize.data.frame <- function(x, method = "mean", select = NULL, exclude = NULL, ...) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  x[select] <- lapply(x[select], standardize, method = method)
  x
}
