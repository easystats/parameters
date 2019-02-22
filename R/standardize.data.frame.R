#' @inherit standardize
#' @importFrom stats median mad
#' @export
standardize.numeric <- function(x, robust = FALSE, ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
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


  if (robust == FALSE) {
    return(as.vector(scale(x, ...)))
  } else {
    return(as.vector((x - median(x, na.rm = TRUE)) / mad(x, na.rm = TRUE)))
  }
}










#' @inherit standardize
#' @export
standardize.factor <- function(x, ...) {
  return(x)
}




#' @inheritParams standardize
#' @export
standardize.grouped_df <- function(x, robust = FALSE, select = NULL, exclude = NULL, ...) {
  grps <- attr(x, "groups", exact = TRUE)
  x <- as.data.frame(x)
  for (i in grps[[".rows"]]) {
    x[i, ] <- standardize(
      x[i, ],
      select = select,
      exclude = exclude,
      robust = robust,
      ...
    )
  }
  # x <- dplyr::do_(x, "standardize(., select = select, exclude = exclude, robust = robust, ...)")
  # return(x)
  x
}


#' Data Standardization
#'
#' Standardize (scale and reduce, Z-score) the data so that the values are expressed in terms of standard deviation (i.e., mean = 0, SD = 1) or Median Absolute Deviance (\code{robust = TRUE}; median = 0, MAD = 1).
#'
#' @inheritParams standardize
#' @param select For a data.frame, character or list of characters of column names to be
#' standardized.
#' @param exclude For a data.frame, character or list of characters of column names to be excluded
#' from standardization.
#'
#' @examples
#' summary(standardize(iris))
#' @export
standardize.data.frame <- function(x, robust = FALSE, select = NULL, exclude = NULL, ...) {
  if (is.null(select)) {
    select <- names(x)
  } else {
    select <- c(select)
  }

  if (!is.null(exclude)) {
    select <- select[!select %in% c(exclude)]
  }

  # TODO: find a base alternative to remove purrr from deps

  # This doesn't work tho:

  # x[select] <- mapply(standardize, x[select], MoreArgs = list(robust = robust))
  # x[select] <- sapply(x[select], standardize, robust = robust)

  x[select] <- lapply(x[select], standardize, robust = robust)
  return(x)
}
